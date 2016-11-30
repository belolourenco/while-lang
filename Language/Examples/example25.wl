try
  x := 1
  if y = 2 then
    w := 2
    throw
  else
    y := y + 1
    if y = 3 then
      throw
    else
      skip
    end
  end
catch
  z := 2;
  k := 4
endc