; ModuleID = '<stdin>'
target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"
target triple = "i386-pc-linux-gnu"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This test checks that clamp pointer checks are not removed from code and that 
;;;; @deref function calls be inlined for improving performance.
;;;; 
;;;; Change some RUN and DONTRUN lines to test that checks failing really mnakes test to fail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; RUN: opt -internalize -internalize-public-api-list=main -Oz -adce -S $TEST_SRC -o $OUT_FILE.ll && 
; DONTRUN: opt -Oz -adce -S $TEST_SRC -o $OUT_FILE.ll && 
;
; RUN: echo "Check that smart pointers are not optimized out" &&
;
; RUN: grep -E "(Last|First|Cur)" $OUT_FILE.ll | grep "data_a" &&
; DONTRUN: grep "just something that is never found" $OUT_FILE.ll | grep "data_a" &&
;
; RUN: (grep "@deref" $OUT_FILE.ll ;
; RUN:   ([ $? != 0 ] && echo "Success @deref function was optimized out as expected") || 
; RUN:   (echo "Fail: Deref was not optimized out:" && cat $OUT_FILE.ll && false) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@NULL_i32 = external constant i32
%SmartPtrType = type { i32*, i32*, i32* }

@prevent_too_aggressive_opts = external global i32*

define i32 @main() nounwind {
entry:
  %retval = alloca i32, align 4
  %index = alloca i32, align 4
  %value = alloca i32, align 4

  ; int data_a[ 256 ]
  %data_a.Smart = alloca %SmartPtrType, align 8
  %data_a.SmartPtr = alloca %SmartPtrType*, align 8
  store %SmartPtrType* %data_a.Smart, %SmartPtrType** %data_a.SmartPtr
  %data_a.Smart.Cur = getelementptr %SmartPtrType* %data_a.Smart, i32 0, i32 0
  %data_a.Smart.First = getelementptr %SmartPtrType* %data_a.Smart, i32 0, i32 1
  %data_a.Smart.Last = getelementptr %SmartPtrType* %data_a.Smart, i32 0, i32 2
  %data_a = alloca [256 x i32], align 8
  %data_a.Last = getelementptr [256 x i32]* %data_a, i32 0, i32 255
  store i32* %data_a.Last, i32** %data_a.Smart.Last
  %data_a.First = getelementptr [256 x i32]* %data_a, i32 0, i32 0
  store i32* %data_a.First, i32** %data_a.Smart.First
  store i32* %data_a.First, i32** %data_a.Smart.Cur

  ; int data_b[ 256 ]
  %data_b.Smart = alloca %SmartPtrType, align 8
  %data_b.SmartPtr = alloca %SmartPtrType*, align 8
  store %SmartPtrType* %data_b.Smart, %SmartPtrType** %data_b.SmartPtr
  %data_b.Smart.Cur = getelementptr %SmartPtrType* %data_b.Smart, i32 0, i32 0
  %data_b.Smart.First = getelementptr %SmartPtrType* %data_b.Smart, i32 0, i32 1
  %data_b.Smart.Last = getelementptr %SmartPtrType* %data_b.Smart, i32 0, i32 2
  %data_b = alloca [256 x i32], align 8
  %data_b.Last = getelementptr [256 x i32]* %data_b, i32 0, i32 255
  store i32* %data_b.Last, i32** %data_b.Smart.Last
  %data_b.First = getelementptr [256 x i32]* %data_b, i32 0, i32 0
  store i32* %data_b.First, i32** %data_b.Smart.First
  store i32* %data_b.First, i32** %data_b.Smart.Cur

  ; int* ptr
  %ptr.Smart = alloca %SmartPtrType, align 8
  %ptr.SmartPtr = alloca %SmartPtrType*, align 8
  store %SmartPtrType* %ptr.Smart, %SmartPtrType** %ptr.SmartPtr
  %ptr.Smart.Cur = getelementptr %SmartPtrType* %ptr.Smart, i32 0, i32 0
  %ptr.Smart.First = getelementptr %SmartPtrType* %ptr.Smart, i32 0, i32 1
  %ptr.Smart.Last = getelementptr %SmartPtrType* %ptr.Smart, i32 0, i32 2
  %ptr = alloca i32*, align 8

  ; ptr = data_a
  %arraydecay = getelementptr inbounds [256 x i32]* %data_a, i32 0, i32 0
  store i32* %arraydecay, i32** %ptr, align 4
  %source.Current3 = load i32** %data_a.Smart.Cur
  %source.Last2 = load i32** %data_a.Smart.Last
  %source.First1 = load i32** %data_a.Smart.First
  store i32* %source.Current3, i32** %ptr.Smart.Cur
  store i32* %source.Last2, i32** %ptr.Smart.Last
  store i32* %source.First1, i32** %ptr.Smart.First

  store i32 0, i32* %retval
  store i32 256, i32* %index, align 4
  %agr_val1 = load i32** @prevent_too_aggressive_opts, align 8
  %agr_val2 = load i32* %agr_val1, align 4
  store i32 %agr_val2, i32* %value, align 4

  %0 = load i32* %value, align 4
  %tobool = icmp ne i32 %0, 0
  br i1 %tobool, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  %arraydecay1 = getelementptr inbounds [256 x i32]* %data_b, i32 0, i32 0
  store i32* %arraydecay1, i32** %ptr, align 4
  %source.Current = load i32** %data_b.Smart.Cur
  %source.Last = load i32** %data_b.Smart.Last
  %source.First = load i32** %data_b.Smart.First
  store i32* %source.Current, i32** %ptr.Smart.Cur
  store i32* %source.Last, i32** %ptr.Smart.Last
  store i32* %source.First, i32** %ptr.Smart.First
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  %1 = load i32** %ptr, align 8
  %clamped = call i32* @deref(i32* %1, %SmartPtrType* %ptr.Smart, i32 202)
  %2 = load i32* %clamped, align 4
  store i32 %2, i32* %value, align 4
  %3 = load i32* %value, align 4
  store i32* @NULL_i32, i32** %data_a.Smart.Cur
  store i32* @NULL_i32, i32** %data_a.Smart.First
  store i32* @NULL_i32, i32** %data_a.Smart.Last
  store i32* @NULL_i32, i32** %data_b.Smart.Cur
  store i32* @NULL_i32, i32** %data_b.Smart.First
  store i32* @NULL_i32, i32** %data_b.Smart.Last
  store i32* @NULL_i32, i32** %ptr.Smart.Cur
  store i32* @NULL_i32, i32** %ptr.Smart.First
  store i32* @NULL_i32, i32** %ptr.Smart.Last
  ret i32 %3
}

; compute pointer address using given %pointer as base address, offset by %index and clamped to %range
; returns clamp(%pointer + %index, %range.min, %range.max)

;;; compiled from deref.c
define i32* @deref(i32* %ptr, %SmartPtrType* nocapture %range, i32 %index) nounwind uwtable readonly optsize ssp minsize {
entry:
  %idx.ext = sext i32 %index to i64
  %add.ptr = getelementptr inbounds i32* %ptr, i64 %idx.ext
  %last = getelementptr inbounds %SmartPtrType* %range, i64 0, i32 2
  %0 = load i32** %last, align 8
  %cmp = icmp ugt i32* %add.ptr, %0
  br i1 %cmp, label %return, label %if.end

if.end:                                           ; preds = %entry
  %first = getelementptr inbounds %SmartPtrType* %range, i64 0, i32 1
  %1 = load i32** %first, align 8
  %cmp2 = icmp ult i32* %add.ptr, %1
  %.add.ptr = select i1 %cmp2, i32* %1, i32* %add.ptr
  br label %return

return:                                           ; preds = %if.end, %entry
  %retval.0 = phi i32* [ %0, %entry ], [ %.add.ptr, %if.end ]
  ret i32* %retval.0
}
