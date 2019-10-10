; ModuleID = 'lib.c'
source_filename = "lib.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@.str.3 = private unnamed_addr constant [5 x i8] c"%lld\00", align 1

; Function Attrs: nounwind ssp uwtable
define i8* @ss_append(i8*, i8*) local_unnamed_addr #0 {
  %3 = tail call i64 @strlen(i8* %0)
  %4 = tail call i64 @strlen(i8* %1)
  %5 = add nsw i64 %4, %3
  %6 = add nsw i64 %5, 1
  %7 = tail call i8* @malloc(i64 %6) #5
  %8 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %7, i1 false, i1 true, i1 false)
  %9 = tail call i8* @__memcpy_chk(i8* %7, i8* %0, i64 %3, i64 %8) #6
  %10 = getelementptr inbounds i8, i8* %7, i64 %3
  %11 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %10, i1 false, i1 true, i1 false)
  %12 = tail call i8* @__memcpy_chk(i8* %10, i8* %1, i64 %4, i64 %11) #6
  %13 = getelementptr inbounds i8, i8* %7, i64 %5
  store i8 0, i8* %13, align 1, !tbaa !4
  ret i8* %7
}

; Function Attrs: argmemonly nounwind readonly
declare i64 @strlen(i8* nocapture) local_unnamed_addr #1

; Function Attrs: nounwind allocsize(0)
declare noalias i8* @malloc(i64) local_unnamed_addr #2

; Function Attrs: nounwind
declare i8* @__memcpy_chk(i8*, i8*, i64, i64) local_unnamed_addr #3

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1, i1, i1) #4

; Function Attrs: nounwind ssp uwtable
define i8* @si64_prefix(i8*, i64) local_unnamed_addr #0 {
  %3 = tail call i64 @strlen(i8* %0)
  %4 = icmp slt i64 %3, %1
  %5 = select i1 %4, i64 %3, i64 %1
  %6 = add nsw i64 %5, 1
  %7 = tail call i8* @malloc(i64 %6) #5
  %8 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %7, i1 false, i1 true, i1 false)
  %9 = tail call i8* @__memcpy_chk(i8* %7, i8* %0, i64 %5, i64 %8) #6
  %10 = getelementptr inbounds i8, i8* %7, i64 %5
  store i8 0, i8* %10, align 1, !tbaa !4
  ret i8* %7
}

; Function Attrs: nounwind ssp uwtable
define i8* @si64i64_slice(i8*, i64, i64) local_unnamed_addr #0 {
  %4 = tail call i64 @strlen(i8* %0)
  %5 = add nsw i64 %2, %1
  %6 = icmp sgt i64 %5, %4
  %7 = select i1 %6, i64 %4, i64 %5
  %8 = sub nsw i64 %7, %1
  %9 = tail call i8* @malloc(i64 %8) #5
  %10 = getelementptr inbounds i8, i8* %0, i64 %1
  %11 = tail call i64 @llvm.objectsize.i64.p0i8(i8* %9, i1 false, i1 true, i1 false)
  %12 = tail call i8* @__memcpy_chk(i8* %9, i8* %10, i64 %7, i64 %11) #6
  %13 = getelementptr inbounds i8, i8* %9, i64 %8
  store i8 0, i8* %13, align 1, !tbaa !4
  ret i8* %9
}

; Function Attrs: nounwind ssp uwtable
define noalias i8* @si64_nth(i8* nocapture readonly, i64) local_unnamed_addr #0 {
  %3 = tail call i8* @malloc(i64 2) #5
  %4 = getelementptr inbounds i8, i8* %0, i64 %1
  %5 = load i8, i8* %4, align 1, !tbaa !4
  store i8 %5, i8* %3, align 1, !tbaa !4
  %6 = getelementptr inbounds i8, i8* %3, i64 1
  store i8 0, i8* %6, align 1, !tbaa !4
  ret i8* %3
}

; Function Attrs: nounwind ssp uwtable
define void @s_printf(i8*) local_unnamed_addr #0 {
  %2 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i8* %0)
  ret void
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #3

; Function Attrs: nounwind ssp uwtable
define void @bool_printf(i8 signext) local_unnamed_addr #0 {
  %2 = icmp eq i8 %0, 0
  %3 = select i1 %2, i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i64 0, i64 0), i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i64 0, i64 0)
  %4 = tail call i32 (i8*, ...) @printf(i8* %3)
  ret void
}

; Function Attrs: nounwind ssp uwtable
define void @i64_printf(i64) local_unnamed_addr #0 {
  %2 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.3, i64 0, i64 0), i64 %0)
  ret void
}

attributes #0 = { nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind readonly "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind readnone speculatable }
attributes #5 = { allocsize(0) }
attributes #6 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 10, i32 15]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.8)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
