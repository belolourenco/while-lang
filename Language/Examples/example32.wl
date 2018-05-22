try
  if x = 1 then
    x := 4
  else
    x := 5;
    throw
  end;
  assert a = 3
catch
  assert y + x = 5
endc
