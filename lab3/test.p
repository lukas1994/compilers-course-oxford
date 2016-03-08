(* lab3/test.p *)

var x;

proc sum(a, b);
begin
  return a+b
end;

proc p1();
begin
  x := 1;
  return x
end;

proc p2();
begin
  x := x*2;
  return x
end;

begin
  (* we evaluate parameters from right to left *)
  x := 0;
  print sum(p1(), p2()); newline;
  x := 0;
  print sum(p2(), p1()); newline
end.

(*<<
 1
 3
>>*)
