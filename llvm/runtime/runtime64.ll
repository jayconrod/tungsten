target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-linux-gnu"

declare noalias i8* @malloc(i64) nounwind
declare void @exit(i32) noreturn nounwind
declare i64 @read(i32, i8*, i64)
declare i64 @write(i32, i8*, i64)
declare i32 @open(i8*, i32)
declare i32 @close(i32)

define i8* @tungsten.malloc(i64 %size) nounwind {
entry:
    %ptr = call noalias i8* @malloc(i64 %size) nounwind
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
