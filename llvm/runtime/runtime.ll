target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-linux-gnu"

declare noalias i8* @malloc(i64) nounwind
declare void @exit(i32) noreturn nounwind

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
