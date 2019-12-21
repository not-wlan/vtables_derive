#![allow(dead_code)]

use core::ptr;
use static_assertions as sa;
use std::os::raw::*;
use vtables::VTable;
use vtables_derive::{has_vtable, virtual_index, VTable};

#[has_vtable]
#[derive(VTable, Debug)]
struct EngineClient {
}

// This structure will fail to compile if #[has_vtable] added another `vtable` field.
#[has_vtable]
struct AlreadyHasVTableField<'a> {
    vtable: u8, 
    foo: bool,
    bar: f32,
    baz: Option<&'a u8>,
}

#[allow(non_snake_case)]
impl EngineClient {
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
    
    let engine_client = EngineClient {
        vtable: ptr::null_mut(),
    };

    let _f = |i| {
        type ExampleVirtualMethod = fn(&EngineClient, bool) -> f64;
        unsafe { engine_client.get_virtual::<ExampleVirtualMethod>(i) };
    };
}

#[test]
fn virtual_index_retains_declared_methods() {
    // This function will fail to compile if #[virtual_index(...)] fails to emit the method it decorates.
    
    let engine_client = EngineClient {
        vtable: ptr::null_mut(),
    };

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