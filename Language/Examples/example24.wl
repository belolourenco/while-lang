try
  x := 1
  if y = 2 then
    w := 2
    throw
  else
    y := y + 1
    throw
  end
catch
  z :=  x + w + y
endc