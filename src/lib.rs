#![warn(clippy::pedantic)]
extern crate proc_macro;

use crate::proc_macro::TokenStream;
use quote::{
    quote,
    ToTokens,
};
use std::ops::Deref;
use syn::{
    self,
    parse::{Parse, Parser, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    Attribute, DeriveInput, ExprLit, Field, FnArg, ItemFn, ItemStruct, Lit, Meta, MetaList, NestedMeta, Pat, Result, Type,
    Fields::Named,
};

fn impl_vtable(ast: DeriveInput) -> TokenStream {
    let DeriveInput { ident, generics, .. } = ast;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let gen = quote! {
        impl #impl_generics VTable for #ident #ty_generics #where_clause {
            unsafe fn get_virtual<__VirtualMethodType: Sized>(&self, index: usize) -> __VirtualMethodType {
                let vtable = self.vtable as *const __VirtualMethodType;
                vtable.add(index).read()
            }
        }
    };

    gen.into()
}

#[proc_macro_derive(VTable)]
pub fn vtable_derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    impl_vtable(ast)
}

fn add_vtable_field(item_struct: &mut ItemStruct) {
    let fields = if let Named(fields) = &mut item_struct.fields {
        fields
    } else {
        // todo: https://docs.rs/syn/1.0.11/syn/struct.Error.html
        panic!("You can only decorate with #[has_vtable] a struct that has named fields.");
    };

    let struct_already_has_vtable_field = fields
    .named
    .iter()
    .any(|f| f
        .ident
        .as_ref()
        .map_or(false, |i| i == "vtable")
    );
    
    if struct_already_has_vtable_field {
        return;
    }
    
    let vtable_field = Field::parse_named
        .parse2(quote! { pub vtable: *mut *mut usize })
        .expect("internal macro error with ill-formatted vtable field");

    fields
        .named
        .insert(0, vtable_field);
}

fn has_repr_c(item_struct: &ItemStruct) -> bool {
    // Look for existing #[repr(C)] variants, e.g.,
    // #[repr(C)]
    // #[repr(C, packed(4))]

    let has = |meta: &Meta, ident| meta
        .path()
        .get_ident()
        .map_or(false, |i| i == ident);

    item_struct
        .attrs
        .iter()
        .filter_map(|a| a
            .parse_meta()
            .ok()
            .filter(|meta| has(meta, "repr"))
            .and_then(|meta| match meta {
                Meta::List(MetaList { nested, .. }) => Some(nested),
                _ => None,
            })
        )
        .flatten()
        .any(|n| match n {
            NestedMeta::Meta(meta) => has(&meta, "C"),
            _ => false,
        })
}

fn add_repr_c(item_struct: &mut ItemStruct) {
    if has_repr_c(item_struct) {
        return;
    }

    let mut repr_c = Attribute::parse_outer
        .parse2(quote! { #[repr(C)] })
        .expect("internal macro error with ill-formed #[repr(C)]");

    item_struct
        .attrs
        .append(&mut repr_c);
}

#[proc_macro_attribute]
pub fn has_vtable(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut parsed: ItemStruct = parse_macro_input!(item as ItemStruct);
    add_vtable_field(&mut parsed);
    add_repr_c(&mut parsed);
    parsed.into_token_stream().into()
}

#[derive(Debug)]
struct VirtualIndex {
    pub index: usize,
}

impl Parse for VirtualIndex {
    fn parse(input: ParseStream) -> Result<Self> {
        let index: ExprLit = input.parse()?;
        if let Lit::Int(int) = &index.lit {
            let idx: usize = int
                .base10_parse()
                .map_err(|_| syn::Error::new(int.span(), "Invalid integer"))?;
            return Ok(VirtualIndex { index: idx });
        }
        Err(syn::Error::new(
            index.span(),
            "Couldn't parse virtual method index!",
        ))
    }
}

#[proc_macro_attribute]
pub fn virtual_index(attr: TokenStream, item: TokenStream) -> TokenStream {
    let index = parse_macro_input!(attr as VirtualIndex).index;

    // Sort out all of these into separate variables since quote! doesn't support
    // the . operator
    let parsed: ItemFn = syn::parse(item.clone()).unwrap();
    // The visibility level of an item: inherited or pub or pub(restricted).
    let vis = &parsed.vis;
    // A vector containing all attributes of this function
    let attrs = &parsed.attrs;
    // A function signature in a trait or implementation: unsafe fn initialize(&self).
    let sig = &parsed.sig;
    // Return type of a function signature.
    let retty = &sig.output;

    // Separate the types from the names in the function parameters.
    // We need the types to define our new function and the names to call it.
    // The self parameter is being ignored here since it's way easier to just hardcode it in the quote!

    // A vector of all parameter types
    let argtys: Vec<Type> = sig
        .inputs
        .iter()
        .flat_map(|arg| {
            if let FnArg::Typed(pat) = arg {
                return Some(pat.ty.deref().clone());
            }
            None
        })
        .collect();

    // A vector of all parameter names
    let args = &sig
        .inputs
        .iter()
        .flat_map(|arg| {
            if let FnArg::Typed(pat) = arg {
                if let Pat::Ident(ident) = pat.pat.deref() {
                    return Some(ident.ident.clone());
                }
            }
            None
        })
        .collect::<Vec<_>>();

    // TODO: Maybe we should pass the unsafe to the caller?
    let gen = quote! {
        #(#attrs)*
        #vis #sig {
            unsafe { self.get_virtual::<fn(*const Self, #(#argtys),*) #retty>(#index)(self as *const Self, #(#args),*) }
        }
    };

    gen.into()
}