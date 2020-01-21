#![warn(clippy::pedantic)]
#![allow(dead_code)]
#![feature(abi_thiscall)]

use core::{
    ops::{Mul, Deref, Div},
    ptr,
};
use static_assertions as sa;
use std::{
    borrow::Cow,
    fmt::Display,
    os::raw::*,
};
use vtables::VTable;
use vtables_derive::{has_vtable, virtual_index, VTable, dyn_glue, dyn_index};

#[has_vtable]
#[derive(VTable, Debug)]
struct EngineClient {
    test_field: u64,
}

#[dyn_glue]
trait EngineClientTrait {
    #[doc = "abc"]
    #[dyn_index(12)]
    fn test_fn() -> usize;
    #[dyn_index(2)]
    fn test_fn_2() -> usize;
    #[dyn_index(3)]
    fn test_fn_3(t: usize);
}

// ================================================================================================
// These structures will fail to compile if #[derive(VTable)] does not respect
// their generics.

#[has_vtable]
#[derive(VTable)]
struct StructWithLifetimes<'a, 'b, 'c> {
    field_with_lifetime_a: Option<&'a u32>,
    field_with_lifetime_b: Cow<'b, str>,
    field_with_lifetime_c: &'c f64,
}

#[has_vtable]
#[derive(VTable)]
struct StructWithTypeGenerics<T, U, V> {
    t: T,
    u: U,
    v: V,
}

#[has_vtable]
#[derive(VTable)]
struct StructWithLifetimesAndGenerics<'a, 'b, 'c, T, U, V: Clone> {
    t: &'a T,
    u: Option<&'b U>,
    v: Cow<'c, V>,
}

#[has_vtable]
#[derive(VTable)]
struct StructWithLifetimesAndGenericsAndVTableField<'a, 'b, 'c, T, U, V: Clone> {
    t: &'a T,
    u: Option<&'b U>,
    vtable: usize,
    v: Cow<'c, V>,
}

#[has_vtable]
#[derive(VTable)]
struct StructWithLifetimesAndGenericsAndVTableFieldAndWhereClause<'a, 'b, 'c, T, U, V>
where
    T: Mul + Div,
    U: AsRef<str> + Copy,
    V: Deref<Target=T> + Clone + Send + Sync + Display,
{
    t: &'a T,
    u: Option<&'b U>,
    vtable: usize,
    v: Cow<'c, V>,
}
// ================================================================================================

// This structure will fail to compile if #[has_vtable] added another `vtable` field.
#[has_vtable]
struct AlreadyHasVTableField<'a> {
    vtable: u8, 
    foo: bool,
    bar: f32,
    baz: Option<&'a u8>,
}

impl Default for EngineClient {
    fn default() -> Self {
        Self {
            vtable: ptr::null_mut(),
            test_field: 0,
        }
    }
}

#[allow(non_snake_case)]
impl EngineClient {
    #[virtual_index(0)]
    pub fn GetTestField(&self) -> u64 {}

    #[virtual_index(1)]
    pub fn MutateTestField(&mut self, new_value: u64) {}

    #[virtual_index(5)]
    pub fn GetScreenSize(&self, width: *mut i32, height: *mut i32) {}
    
    #[virtual_index(26)]
    pub fn IsInGame(&self) -> bool {}

    #[virtual_index(108)]
    pub fn ExecuteClientCmd(&self, command: *const c_char) {}

    #[virtual_index(113)]
    pub fn ClientCmd_Unrestricted(&self, command: *const c_char) {}
}

#[test]
fn has_vtable_adds_vtable_field() {
    // This function will fail to compile if #[has_vtable] does not add a `vtable`.
    sa::assert_fields!(EngineClient: vtable);
}

#[test]
fn derive_vtable_adds_get_virtual_method() {
    // This function will fail to compile if #[derive(VTable)] did not add a `get_virtual(usize)` method.
    
    let engine_client = EngineClient::default();

    let _f = |i| {
        type ExampleVirtualMethod = fn(&EngineClient, bool) -> f64;
        unsafe { engine_client.get_virtual::<ExampleVirtualMethod>(i) };
    };
}

#[test]
fn virtual_index_retains_declared_methods() {
    // This function will fail to compile if #[virtual_index(...)] fails to emit the method it decorates.
    
    let engine_client = EngineClient::default();

    macro_rules! verify {
        ($method:ident) => {{
            verify!($method,)
        }};

        ($method:ident, $($arg:ident),*) => {{
            let _f = |$($arg),*| engine_client.$method($($arg),*);
        }};
    }

    verify!(GetScreenSize, w, h);
    verify!(IsInGame);
    verify!(ExecuteClientCmd, command);
    verify!(ClientCmd_Unrestricted, command);
}

fn new_vtable_with_one_function(function_index: usize, function_pointer: *mut usize) -> Vec<*mut usize> {
    // [null, null, null, ..., function_pointer, null, ...]
    //   0     1     2          function_index   NUM_NULL_ENTRIES_AFTER_FUNCTION
    const NUM_NULL_ENTRIES_AFTER_FUNCTION: usize = 3;
    let num_total_entries = 1 + function_index + NUM_NULL_ENTRIES_AFTER_FUNCTION;
    let mut vtable = vec![ptr::null_mut(); num_total_entries];
    vtable[function_index] = function_pointer;
    vtable
}

#[test]
fn call_void_virtual_method() {
    const WIDTH_ASSERT: i32 = 800;
    const HEIGHT_ASSERT: i32 = 600;

    unsafe fn get_screen_size_impl(_client: &EngineClient, width: *mut i32, height: *mut i32) {
        *width = WIDTH_ASSERT;
        *height = HEIGHT_ASSERT;
    }

    let mut vtable = new_vtable_with_one_function(5, get_screen_size_impl as _);

    let engine_client = EngineClient {
        vtable: vtable.as_mut_ptr(),
        ..Default::default()
    };

    let mut width = 0;
    let mut height = 0;
    engine_client.GetScreenSize(&mut width, &mut height);
 
    assert_eq!(width, WIDTH_ASSERT);
    assert_eq!(height, HEIGHT_ASSERT);
}

#[test]
fn call_virtual_method_to_return_internal_field() {
    const FIELD_ASSERT: u64 = 0xCafeBabe;

    fn get_test_field_impl(client: &EngineClient) -> u64 {
        client.test_field
    }

    let mut vtable = new_vtable_with_one_function(0, get_test_field_impl as _);

    let engine_client = EngineClient {
        vtable: vtable.as_mut_ptr(),
        test_field: FIELD_ASSERT
    };

    assert_eq!(engine_client.GetTestField(), FIELD_ASSERT);
}

#[test]
fn call_virtual_method_to_mutate_internal_field() {
    const FIELD_ASSERT: u64 = 0xDeadBeef;

    fn mutate_test_field_impl(client: &mut EngineClient, new_value: u64) {
        client.test_field = new_value;
    }

    let mut vtable = new_vtable_with_one_function(1, mutate_test_field_impl as _);

    let mut engine_client = EngineClient {
        vtable: vtable.as_mut_ptr(),
        test_field: 0,
    };

    assert_eq!(engine_client.test_field, 0);

    engine_client.MutateTestField(FIELD_ASSERT);

    assert_eq!(engine_client.test_field, FIELD_ASSERT);
}

#[test]
fn check_that_derive_vtable_adds_repr_c() {
    // To be honest, this test is flaky because it relies on struct layouts
    // whose stability I'm not familiar with across platforms, rustc versions, etc.
    // Ideally, there'd be a simpler way to just check if a struct is literally
    // decorated with the #[repr(C)] attribute rather than going through this dance of looking for
    // the effects of the attribute.

    // Anyway, here's the theory:
    // If #[has_vtable] didn't add #[repr(C)] to the struct,
    // then we'd expect to see the Rust compiler laying out the fields according to #[repr(rust)].
    // This default layout should order the fields of the struct in decreasing alignment to minimize padding.
    // With that knowledge, we can probe the struct using raw pointers to look for the unique values we initialize
    // each field with.
    // https://github.com/rust-lang/rust/pull/37429

    macro_rules! z { 
        ($t:ty) => {{ 
            core::mem::size_of::<$t>() 
        }} 
    }

    {
        // First, test the theory on an undecorated struct.
        struct Control {
            least_aligned: u8,
            somewhat_aligned: u16,
            most_aligned: u32,
        }
        
        let control = Control {
            least_aligned: 42,
            somewhat_aligned: 2019,
            most_aligned: 0xCafeBabe,
        };

        /*
            We expect the Rust compiler to layout `control` in memory as:

            +0: 0xCafeBabe
            +4: 2019
            +6: 42
            +7: 1 byte padding to satisfy alignment constraint of the entire struct.

            i.e., decreasing alignment
        */

        unsafe {
            // Verify the fields are in fact laid out in decreasing alignment.
            let cursor = &control as *const _ as *const u8;
            let mut offset = 0;

            {
                let cursor: *const u32 = cursor.add(offset).cast();
                assert_eq!(*cursor, control.most_aligned);
                offset += z!(u32);
            }

            {
                let cursor: *const u16 = cursor.add(offset).cast();
                assert_eq!(*cursor, control.somewhat_aligned);
                offset += z!(u16);
            }
            
            {
                let cursor: *const u8 = cursor.add(offset).cast();
                assert_eq!(*cursor, control.least_aligned);
                offset += z!(u8);
            }

            // 1 byte padding to align entire struct to a multiple of maximum alignment.
            offset += 1;

            assert_eq!(z!(Control), offset);
        }
    }

    // Great, those tests passed. So the theory is okay to work with.
    // Now let's decorate a struct with #[has_vtable] to see if it has the #[repr(C)] attribute.
    // We shouldn't see any of the struct fields moving from their declaration order.

    #[has_vtable]
    #[derive(VTable)]
    struct Control {
        vtable: usize,

        least_aligned: u8,
        somewhat_aligned: u16,
        most_aligned: u32,
    }

    let control = Control {
        vtable: 0,

        least_aligned: 42,
        somewhat_aligned: 2019,
        most_aligned: 0xCafeBabe,
    };

    /*
        We expect #[repr(C)] to layout `control` in memory as:

        +0:     0

        +8:     42
        +9:     1 byte padding to satisfy alignment requirement of 2 bytes for `somewhat_aligned`
        +10:    2019
        +12:    0xCafeBabe

        i.e., declaration order with padding to satisfy alignment requirements
    */

    unsafe {
        // Verify the fields are in declaration order.
        let cursor = &control as *const _ as *const u8;
        let mut offset = z!(usize); // skip past vtable usize

        {
            let cursor: *const u8 = cursor.add(offset).cast();
            assert_eq!(*cursor, control.least_aligned);
            offset += z!(u8);
        }

        {
            offset += 1; // 1 byte padding
            let cursor: *const u16 = cursor.add(offset).cast();
            assert_eq!(*cursor, control.somewhat_aligned);
            offset += z!(u16);
        }
        
        {
            let cursor: *const u32 = cursor.add(offset).cast();
            assert_eq!(*cursor, control.most_aligned);
            offset += z!(u32);
        }
        
        assert_eq!(z!(Control), offset);
    }

    // Sanity check on alignment ordering.
    
    macro_rules! a { 
        ($t:ty) => {{ 
            core::mem::align_of::<$t>() 
        }} 
    }

    let alignments = [a!(u8), a!(u16), a!(u32)];

    assert_eq!(*alignments.iter().min().unwrap(), a!(u8), "u8 should have the smallest alignment constraint.");
    assert_eq!(*alignments.iter().max().unwrap(), a!(u32), "u32 should have the largest alignment constraint.");
}