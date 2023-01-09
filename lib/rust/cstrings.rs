use std::ffi::*;
// use std::ffi::CStr;
// use std::ffi::CString;
// use std::os::raw::c_char;

let (host, pass) = ("127.0.0.1:1491", "dmdm");
let cs = CString::new(host).unwrap();
let p = cs.as_ptr();
let cs = CString::new(pass).unwrap();
let pp = cs.as_ptr();

fn to_str(s: *const u8) -> String {
    unsafe {
        let host = CStr::from_ptr(s as *const i8).to_str().unwrap();
        return host.to_string();
    };
}
