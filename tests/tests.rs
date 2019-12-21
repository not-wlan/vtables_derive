use static_assertions as sa;
use std::os::raw::*;
use vtables::VTable;
use vtables_derive::{has_vtable, virtual_index, VTable};

#[has_vtable]
#[derive(VTable, Debug)]
pub struct EngineClient {
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
fn pass() {
    assert!(true);
}