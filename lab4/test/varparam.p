(* Access to var parameters from nested procedures *)

proc one(var x: integer);
  proc two(); begin x := x+1 end;
begin
  two()
end;

proc three();
  var y: integer;
begin
  y := 36;
  one(y);
  print_num(y);
  newline()
end;

begin
  three()
end.

(*<<
37
>>*)

(*[[
@ picoPascal compiler output
	.global pmain

@ proc one(var x: integer);
	.text
_one:
	mov ip, sp
	stmfd sp!, {r0-r1}
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   two()
	mov r4, fp
	bl _two
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@   proc two(); begin x := x+1 end;
_two:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   proc two(); begin x := x+1 end;
	ldr r0, [fp]
	ldr r5, [r0, #40]
	ldr r0, [r5]
	add r0, r0, #1
	str r0, [r5]
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ proc three();
_three:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
	sub sp, sp, #8
@   y := 36;
	mov r0, #36
	str r0, [fp, #-4]
@   one(y);
	add r0, fp, #-4
	bl _one
@   print_num(y);
	ldr r0, [fp, #-4]
	bl print_num
@   newline()
	bl newline
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

pmain:
	mov ip, sp
	stmfd sp!, {r4-r10, fp, ip, lr}
	mov fp, sp
@   three()
	bl _three
	ldmfd fp, {r4-r10, fp, sp, pc}
	.ltorg

@ End
]]*)