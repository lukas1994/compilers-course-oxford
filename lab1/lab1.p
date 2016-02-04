(* lab1/lab1.p *)

begin
  (* empty body *)
  repeat until 1 > 0;

  (* extra semicolon at the end of the repeat body *)
  i := 0;
  repeat
    print i; newline;
    i := i + 1;
  until i > 1;

  (* extra semicolon at the end of the loop body *)
  i := 0;
  loop
    print i; newline;
    i := i + 1;
    if i > 1 then exit end;
  end;

  (* allow empty case, omit else *)
  i := 0;
  case i of
    1, 2, 3:
    | 0: print i; newline;
  end;

  (* allow empty else *)
  i := 0;
  case i of
    1, 2, 3:
    else print i; newline;
  end;
end.

(*<<
 0
 1
 0
 1
 0
 0
>>*)
