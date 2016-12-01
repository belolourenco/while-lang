try
  if x = y then
    throw
  else
    x := 1
  end;
  z := 2
catch
  y := z + x
endc;
z := x + y + z