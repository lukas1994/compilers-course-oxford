(* lab2/test.p *)

var a: array 4 of boolean;
var b: array 2 of integer;

begin
  (** test STOREC/STOREW **)
  (* set a to true *)
  a[0] := (0 <> 1); a[1] := (0 <> 1); a[2] := (0 <> 1); a[3] := (0 <> 1);
  (* set a[0] to false *)
  a[0] := (0 <> 0);
  if a[1] then print 999; newline
  else print 111; newline end;

  (** test a[b[i]] **)
  b[0] := 1; b[1] := 1000;
  print b[b[0]]; newline
end.

(*<<
 999
 1000
>>*)
