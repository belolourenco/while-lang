try
  if x = 2 then
    assume x = 4;
    throw
  else
    y := 3
  end
catch
  assert y + x = 5
endc