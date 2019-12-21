extern crate proc_macro;

use crate::proc_macro::TokenStream;
use quote::quote;
use std::ops::Deref;
use syn::{
    self,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    DeriveInput, ExprLit, FnArg, ItemFn, ItemStruct, Lit, Pat, Result, Type,
};

fn impl_vtable(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl VTable for #name {
            unsafe fn get_virtual<T: Sized>(&self, index: usize) -> T {
                ((self.vtable as *mut *mut usize).add(index) as *const T).read()
            }
        }
    };
    gen.into()
}

#[proc_macro_derive(VTable)]
pub fn vtable_derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    impl_vtable(&ast)
}

#[proc_macro_attribute]
pub fn has_vtable(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let parsed: ItemStruct = syn::parse(item).unwrap();

    let name = &parsed.ident;
    let attrs = &parsed.attrs;
    let fields = &parsed.fields.iter().collect::<Vec<_>>();
    let vis = &parsed.vis;

    let vtable = fields.iter().find(|field| {
        if let Some(ident) = &field.ident {
            return &ident.to_string() == "vtable";
        }
        false
    });

    if let Some(_) = vtable {
        let gen = quote! {
            #parsed
        };

        return gen.into();
    }

    // Adds #[repr(C)] so we don't accidentally fuck up the padding.
    // Adds the necessary vtable field as first element
    // In theory I could make this add a #[derive(VTable)] but that'd require parsing the attributes first
    let gen = quote! {
        #[repr(C)]
        #(#attrs)*
        #vis struct #name {
            pub vtable: *mut *mut usize,
            #(#fields),*
        }
    };

    gen.into()
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