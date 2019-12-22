#![warn(clippy::pedantic)]
#![allow(dead_code)]

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
use vtables_derive::{has_vtable, virtual_index, VTable};

#[has_vtable]
#[derive(VTable, Debug)]
struct EngineClient {
    test_field: u64,
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