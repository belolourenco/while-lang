try
  if x = y then
    y := 1;
    assume y > 2;
    throw
  else
    x := 1
  end;
  z := x + y
catch
  y := z + x + y;
  assert y > 5
endc;
z := x + y + z;
assert x = 10