target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128-n8:16:32"
target triple = "i386-linux-gnu"

%struct.ctor = type { i32, void ()* }

@llvm.global_ctors = appending global [1 x %struct.ctor] [%struct.ctor { i32 65535, void ()* @tungsten.install_signal_handler }]
@_ZTIPv = external constant i8*

declare noalias i8* @malloc(i64) nounwind
declare void @exit(i32) noreturn nounwind
declare i64 @read(i32, i8*, i64)
declare i64 @write(i32, i8*, i64)
declare i32 @open(i8*, i32)
declare i32 @close(i32)
declare void (i32)* @signal(i32, void (i32)*) nounwind
declare i8* @__cxa_allocate_exception(i32) nounwind
declare void @__cxa_throw(i8*, i8*, i8*)

define i8* @tungsten.malloc(i32 %size) nounwind {
entry:
    %sizeext = zext i32 %size to i64
    %ptr = call noalias i8* @malloc(i64 %sizeext) nounwind
    ret i8* %ptr
}

define void @tungsten.exit(i32 %code) noreturn nounwind {
entry:
    call void @exit(i32 %code) noreturn nounwind
    unreachable
}

define i64 @tungsten.read(i32 %fd, i8* %buffer, i64 %size) {
entry:
    %ret = call i64 @read(i32 %fd, i8* %buffer, i64 %size)
    ret i64 %ret
}

define i64 @tungsten.write(i32 %fd, i8* %buffer, i64 %size) {
entry:
    %ret = call i64 @write(i32 %fd, i8* %buffer, i64 %size)
    ret i64 %ret
}

define i32 @tungsten.open(i8* %filename, i32 %flags) {
entry:
    %ret = call i32 @open(i8* %filename, i32 %flags)
    ret i32 %ret
}

define i32 @tungsten.close(i32 %fd) {
entry:
    %ret = call i32 @close(i32 %fd)
    ret i32 %ret
}

define void @tungsten.install_signal_handler() {
entry:
    %ret = call void (i32)* (i32, void (i32)*)* @signal(i32 11, void (i32)* @tungsten.signal_handler) nounwind
    ret void
}

define void @tungsten.signal_handler(i32 %signum) {
entry:
    %0 = call i8* @__cxa_allocate_exception(i32 8) nounwind
    %1 = bitcast i8* %0 to i8**
    store i8* null, i8** %1
    call void @__cxa_throw(i8* %0, i8* bitcast (i8** @_ZTIPv to i8*), i8* null) noreturn
    unreachable
}

