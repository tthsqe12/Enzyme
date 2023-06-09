; RUN: if [ %llvmver -lt 16 ]; then %opt < %s %loadEnzyme -enzyme -S | FileCheck %s; fi
; RUN: %opt < %s %newLoadEnzyme -passes="enzyme" -S | FileCheck %s

@.str = private constant [11 x i8] c"predict, 0\00"
@.str.1 = private constant [2 x i8] c"m\00"
@.str.2 = private constant [2 x i8] c"b\00"

@enzyme_observations = global i32 0
@enzyme_interface = global i32 0
@enzyme_trace = global i32 0
@enzyme_const = global i32 0

declare double @normal(double, double)
declare double @normal_logpdf(double, double, double)

declare double @exp(double)
declare double @log(double)

declare double @__enzyme_sample(double (double, double)*, double (double, double, double)*, i8*, double, double)
declare double @__enzyme_condition(double (double*, i32)*, i32, double*, i32, i32, i8*, i32, i8*, i32, i8**)

declare i8* @__enzyme_newtrace()


define double @calculate_loss(double %m, double %b, double* %data, i32 %n) {
entry:
  %cmp19 = icmp sgt i32 %n, 0
  br i1 %cmp19, label %for.body.preheader, label %for.cond.cleanup

for.body.preheader:                               ; preds = %entry
  %wide.trip.count = zext i32 %n to i64
  br label %for.body

for.cond.cleanup:                                 ; preds = %for.body, %entry
  %loss.0.lcssa = phi double [ 0.0, %entry ], [ %3, %for.body ]
  ret double %loss.0.lcssa

for.body:                                         ; preds = %for.body.preheader, %for.body
  %indvars.iv = phi i64 [ 0, %for.body.preheader ], [ %indvars.iv.next, %for.body ]
  %loss.021 = phi double [ 0.0, %for.body.preheader ], [ %3, %for.body ]
  %0 = trunc i64 %indvars.iv to i32
  %conv2 = sitofp i32 %0 to double
  %mul1 = fmul double %conv2, %m
  %1 = fadd double %mul1, %b 
  %call = tail call double @__enzyme_sample(double (double, double)* @normal, double (double, double, double)* @normal_logpdf, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str, i64 0, i64 0), double %1, double 1.0)
  %arrayidx3 = getelementptr inbounds double, double* %data, i64 %indvars.iv
  %2 = load double, double* %arrayidx3
  %sub = fsub double %call, %2
  %mul2 = fmul double %sub, %sub
  %3 = fadd double %mul2, %loss.021
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %exitcond.not = icmp eq i64 %indvars.iv.next, %wide.trip.count
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
}

define double @loss(double* %data, i32 %n) {
entry:
  %call = tail call double @__enzyme_sample(double (double, double)* @normal, double (double, double, double)* @normal_logpdf, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0), double 0.0, double 1.0)
  %call1 = tail call double @__enzyme_sample(double (double, double)* @normal, double (double, double, double)* @normal_logpdf, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i64 0, i64 0), double 0.0, double 1.0)
  %call2 = tail call double @calculate_loss(double %call, double %call1, double* %data, i32 %n)
  ret double %call2
}

define i8* @condition(double* %data, i32 %n, i8* %observations, i8** %interface) {
entry:
  %0 = load i32, i32* @enzyme_observations
  %1 = load i32, i32* @enzyme_interface
  %2 = load i32, i32* @enzyme_trace
  %3 = load i32, i32* @enzyme_const
  %trace = call i8* @__enzyme_newtrace()
  %call = tail call double @__enzyme_condition(double (double*, i32)* @loss, i32 %3, double* %data, i32 %n, i32 %0, i8* %observations, i32 %2, i8* %trace, i32 %1, i8** %interface)
  ret i8* %trace
}

; CHECK: define internal double @condition_loss(double* %data, i32 %n, double* "enzyme_likelihood" %likelihood, i8* "enzyme_observations" %observations, i8* "enzyme_trace" %trace)
; CHECK-NEXT: entry:
; CHECK-NEXT:   %call.ptr.i4 = alloca double
; CHECK-NEXT:   %call.ptr.i = alloca double
; CHECK-NEXT:   %0 = load void (i8*, i8*)*, void (i8*, i8*)** @insert_function_ptr
; CHECK-NEXT:   call void %0(i8* %trace, i8* bitcast (double (double*, i32, double*, i8*, i8*)* @condition_loss to i8*))
; CHECK-NEXT:   %1 = bitcast double* %data to i8*
; CHECK-NEXT:   call void @insert_argument(i8* %trace, i8* nocapture readonly getelementptr inbounds ([5 x i8], [5 x i8]* @0, i32 0, i32 0), i8* %1, i64 0)
; CHECK-NEXT:   %2 = zext i32 %n to i64
; CHECK-NEXT:   %3 = inttoptr i64 %2 to i8*
; CHECK-NEXT:   call void @insert_argument(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @1, i32 0, i32 0), i8* %3, i64 4)
; CHECK-NEXT:   %4 = bitcast double* %call.ptr.i to i8*
; CHECK-NEXT:   call void @llvm.lifetime.start.p0i8(i64 8, i8* %4)
; CHECK-NEXT:   %has.choice.call.i = call i1 @has_choice(i8* %observations, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0))
; CHECK-NEXT:   br i1 %has.choice.call.i, label %condition.call.with.trace.i, label %condition.call.without.trace.i

; CHECK: condition.call.with.trace.i:                      ; preds = %entry
; CHECK-NEXT:   %5 = bitcast double* %call.ptr.i to i8*
; CHECK-NEXT:   %call.size.i = call i64 @get_choice(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0), i8* %5, i64 8)
; CHECK-NEXT:   %from.trace.call.i = load double, double* %call.ptr.i
; CHECK-NEXT:   br label %sample_or_condition.exit

; CHECK: condition.call.without.trace.i:                   ; preds = %entry
; CHECK-NEXT:   %sample.call.i = call double @normal(double 0.000000e+00, double 1.000000e+00)
; CHECK-NEXT:   br label %sample_or_condition.exit

; CHECK: sample_or_condition.exit:                         ; preds = %condition.call.with.trace.i, %condition.call.without.trace.i
; CHECK-NEXT:   %6 = phi double [ %from.trace.call.i, %condition.call.with.trace.i ], [ %sample.call.i, %condition.call.without.trace.i ]
; CHECK-NEXT:   %7 = bitcast double* %call.ptr.i to i8*
; CHECK-NEXT:   call void @llvm.lifetime.end.p0i8(i64 8, i8* %7)
; CHECK-NEXT:   %likelihood.call = call double @normal_logpdf(double 0.000000e+00, double 1.000000e+00, double %6)
; CHECK-NEXT:   %log_prob_sum = load double, double* %likelihood
; CHECK-NEXT:   %8 = fadd double %log_prob_sum, %likelihood.call
; CHECK-NEXT:   store double %8, double* %likelihood
; CHECK-NEXT:   %9 = bitcast double %6 to i64
; CHECK-NEXT:   %10 = inttoptr i64 %9 to i8*
; CHECK-NEXT:   call void @insert_choice(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0), double %likelihood.call, i8* %10, i64 8)
; CHECK-NEXT:   %11 = bitcast double* %call.ptr.i4 to i8*
; CHECK-NEXT:   call void @llvm.lifetime.start.p0i8(i64 8, i8* %11)
; CHECK-NEXT:   %has.choice.call.i5 = call i1 @has_choice(i8* %observations, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i64 0, i64 0))
; CHECK-NEXT:   br i1 %has.choice.call.i5, label %condition.call.with.trace.i8, label %condition.call.without.trace.i10

; CHECK: condition.call.with.trace.i8:                     ; preds = %sample_or_condition.exit
; CHECK-NEXT:   %12 = bitcast double* %call.ptr.i4 to i8*
; CHECK-NEXT:   %call.size.i6 = call i64 @get_choice(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i64 0, i64 0), i8* %12, i64 8)
; CHECK-NEXT:   %from.trace.call.i7 = load double, double* %call.ptr.i4
; CHECK-NEXT:   br label %sample_or_condition.exit11

; CHECK: condition.call.without.trace.i10:                 ; preds = %sample_or_condition.exit
; CHECK-NEXT:   %sample.call.i9 = call double @normal(double 0.000000e+00, double 1.000000e+00)
; CHECK-NEXT:   br label %sample_or_condition.exit11

; CHECK: sample_or_condition.exit11:                       ; preds = %condition.call.with.trace.i8, %condition.call.without.trace.i10
; CHECK-NEXT:   %13 = phi double [ %from.trace.call.i7, %condition.call.with.trace.i8 ], [ %sample.call.i9, %condition.call.without.trace.i10 ]
; CHECK-NEXT:   %14 = bitcast double* %call.ptr.i4 to i8*
; CHECK-NEXT:   call void @llvm.lifetime.end.p0i8(i64 8, i8* %14)
; CHECK-NEXT:   %likelihood.call1 = call double @normal_logpdf(double 0.000000e+00, double 1.000000e+00, double %13)
; CHECK-NEXT:   %log_prob_sum1 = load double, double* %likelihood
; CHECK-NEXT:   %15 = fadd double %log_prob_sum1, %likelihood.call1
; CHECK-NEXT:   store double %15, double* %likelihood
; CHECK-NEXT:   %16 = bitcast double %13 to i64
; CHECK-NEXT:   %17 = inttoptr i64 %16 to i8*
; CHECK-NEXT:   call void @insert_choice(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i64 0, i64 0), double %likelihood.call1, i8* %17, i64 8)
; CHECK-NEXT:   %18 = load i8* ()*, i8* ()** @new_trace_ptr
; CHECK-NEXT:   %19 = call i8* %18()
; CHECK-NEXT:   %20 = load i1 (i8*, i8*)*, i1 (i8*, i8*)** @has_call_ptr
; CHECK-NEXT:   %21 = call i1 %20(i8* %observations, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @2, i32 0, i32 0))
; CHECK-NEXT:   br i1 %21, label %condition.call2.with.trace, label %condition.call2.without.trace

; CHECK: condition.call2.with.trace:                       ; preds = %sample_or_condition.exit11
; CHECK-NEXT:   %22 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** @get_trace_ptr
; CHECK-NEXT:   %23 = call i8* %22(i8* %observations, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @2, i32 0, i32 0))
; CHECK-NEXT:   %condition.calculate_loss = call double @condition_calculate_loss(double %6, double %13, double* %data, i32 %n, double* %likelihood, i8* %23, i8* %19)
; CHECK-NEXT:   br label %entry.cntd

; CHECK: condition.call2.without.trace:                    ; preds = %sample_or_condition.exit11
; CHECK-NEXT:   %trace.calculate_loss = call double @condition_calculate_loss(double %6, double %13, double* %data, i32 %n, double* %likelihood, i8* null, i8* %19)
; CHECK-NEXT:   br label %entry.cntd

; CHECK: entry.cntd:                                       ; preds = %condition.call2.without.trace, %condition.call2.with.trace
; CHECK-NEXT:   %call2 = phi double [ %condition.calculate_loss, %condition.call2.with.trace ], [ %trace.calculate_loss, %condition.call2.without.trace ]
; CHECK-NEXT:   %24 = load void (i8*, i8*, i8*)*, void (i8*, i8*, i8*)** @insert_call_ptr
; CHECK-NEXT:   call void %24(i8* %trace, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @2, i32 0, i32 0), i8* %19)
; CHECK-NEXT:   %25 = bitcast double %call2 to i64
; CHECK-NEXT:   %26 = inttoptr i64 %25 to i8*
; CHECK-NEXT:   %27 = load void (i8*, i8*, i64)*, void (i8*, i8*, i64)** @insert_return_ptr
; CHECK-NEXT:   call void %27(i8* %trace, i8* %26, i64 8)
; CHECK-NEXT:   ret double %call2
; CHECK-NEXT: }

; CHECK: define internal double @condition_calculate_loss(double %m, double %b, double* %data, i32 %n, double* "enzyme_likelihood" %likelihood, i8* "enzyme_observations" %observations, i8* "enzyme_trace" %trace) {
; CHECK-NEXT: entry:
; CHECK-NEXT:   %call.ptr.i = alloca double
; CHECK-NEXT:   %0 = load void (i8*, i8*)*, void (i8*, i8*)** @insert_function_ptr
; CHECK-NEXT:   call void %0(i8* %trace, i8* bitcast (double (double, double, double*, i32, double*, i8*, i8*)* @condition_calculate_loss to i8*))
; CHECK-NEXT:   %1 = bitcast double %m to i64
; CHECK-NEXT:   %2 = inttoptr i64 %1 to i8*
; CHECK-NEXT:   call void @insert_argument(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @3, i32 0, i32 0), i8* %2, i64 8)
; CHECK-NEXT:   %3 = bitcast double %b to i64
; CHECK-NEXT:   %4 = inttoptr i64 %3 to i8*
; CHECK-NEXT:   call void @insert_argument(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @4, i32 0, i32 0), i8* %4, i64 8)
; CHECK-NEXT:   %5 = bitcast double* %data to i8*
; CHECK-NEXT:   call void @insert_argument(i8* %trace, i8* nocapture readonly getelementptr inbounds ([5 x i8], [5 x i8]* @5, i32 0, i32 0), i8* %5, i64 0)
; CHECK-NEXT:   %6 = zext i32 %n to i64
; CHECK-NEXT:   %7 = inttoptr i64 %6 to i8*
; CHECK-NEXT:   call void @insert_argument(i8* %trace, i8* nocapture readonly getelementptr inbounds ([2 x i8], [2 x i8]* @6, i32 0, i32 0), i8* %7, i64 4)
; CHECK-NEXT:   %cmp19 = icmp sgt i32 %n, 0
; CHECK-NEXT:   br i1 %cmp19, label %for.body.preheader, label %for.cond.cleanup

; CHECK: for.body.preheader:                               ; preds = %entry
; CHECK-NEXT:   %wide.trip.count = zext i32 %n to i64
; CHECK-NEXT:   br label %for.body

; CHECK: for.cond.cleanup:                                 ; preds = %sample_or_condition.exit, %entry
; CHECK-NEXT:   %loss.0.lcssa = phi double [ 0.000000e+00, %entry ], [ %21, %sample_or_condition.exit ]
; CHECK-NEXT:   %8 = bitcast double %loss.0.lcssa to i64
; CHECK-NEXT:   %9 = inttoptr i64 %8 to i8*
; CHECK-NEXT:   %10 = load void (i8*, i8*, i64)*, void (i8*, i8*, i64)** @insert_return_ptr
; CHECK-NEXT:   call void %10(i8* %trace, i8* %9, i64 8)
; CHECK-NEXT:   ret double %loss.0.lcssa

; CHECK: for.body:                                         ; preds = %sample_or_condition.exit, %for.body.preheader
; CHECK-NEXT:   %indvars.iv = phi i64 [ 0, %for.body.preheader ], [ %indvars.iv.next, %sample_or_condition.exit ]
; CHECK-NEXT:   %loss.021 = phi double [ 0.000000e+00, %for.body.preheader ], [ %21, %sample_or_condition.exit ]
; CHECK-NEXT:   %11 = trunc i64 %indvars.iv to i32
; CHECK-NEXT:   %conv2 = sitofp i32 %11 to double
; CHECK-NEXT:   %mul1 = fmul double %conv2, %m
; CHECK-NEXT:   %12 = fadd double %mul1, %b
; CHECK-NEXT:   %13 = bitcast double* %call.ptr.i to i8*
; CHECK-NEXT:   call void @llvm.lifetime.start.p0i8(i64 8, i8* %13)
; CHECK-NEXT:   %has.choice.call.i = call i1 @has_choice(i8* %observations, i8* nocapture readonly getelementptr inbounds ([11 x i8], [11 x i8]* @.str, i64 0, i64 0))
; CHECK-NEXT:   br i1 %has.choice.call.i, label %condition.call.with.trace.i, label %condition.call.without.trace.i

; CHECK: condition.call.with.trace.i:                      ; preds = %for.body
; CHECK-NEXT:   %14 = bitcast double* %call.ptr.i to i8*
; CHECK-NEXT:   %call.size.i = call i64 @get_choice(i8* %trace, i8* nocapture readonly getelementptr inbounds ([11 x i8], [11 x i8]* @.str, i64 0, i64 0), i8* %14, i64 8)
; CHECK-NEXT:   %from.trace.call.i = load double, double* %call.ptr.i
; CHECK-NEXT:   br label %sample_or_condition.exit

; CHECK: condition.call.without.trace.i:                   ; preds = %for.body
; CHECK-NEXT:   %sample.call.i = call double @normal(double %12, double 1.000000e+00)
; CHECK-NEXT:   br label %sample_or_condition.exit

; CHECK: sample_or_condition.exit:                         ; preds = %condition.call.with.trace.i, %condition.call.without.trace.i
; CHECK-NEXT:   %15 = phi double [ %from.trace.call.i, %condition.call.with.trace.i ], [ %sample.call.i, %condition.call.without.trace.i ]
; CHECK-NEXT:   %16 = bitcast double* %call.ptr.i to i8*
; CHECK-NEXT:   call void @llvm.lifetime.end.p0i8(i64 8, i8* %16)
; CHECK-NEXT:   %likelihood.call = call double @normal_logpdf(double %12, double 1.000000e+00, double %15)
; CHECK-NEXT:   %log_prob_sum = load double, double* %likelihood
; CHECK-NEXT:   %17 = fadd double %log_prob_sum, %likelihood.call
; CHECK-NEXT:   store double %17, double* %likelihood
; CHECK-NEXT:   %18 = bitcast double %15 to i64
; CHECK-NEXT:   %19 = inttoptr i64 %18 to i8*
; CHECK-NEXT:   call void @insert_choice(i8* %trace, i8* nocapture readonly getelementptr inbounds ([11 x i8], [11 x i8]* @.str, i64 0, i64 0), double %likelihood.call, i8* %19, i64 8)
; CHECK-NEXT:   %arrayidx3 = getelementptr inbounds double, double* %data, i64 %indvars.iv
; CHECK-NEXT:   %20 = load double, double* %arrayidx3
; CHECK-NEXT:   %sub = fsub double %15, %20
; CHECK-NEXT:   %mul2 = fmul double %sub, %sub
; CHECK-NEXT:   %21 = fadd double %mul2, %loss.021
; CHECK-NEXT:   %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
; CHECK-NEXT:   %exitcond.not = icmp eq i64 %indvars.iv.next, %wide.trip.count
; CHECK-NEXT:   br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
; CHECK-NEXT: }