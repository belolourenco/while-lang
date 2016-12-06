try
  if x = 1 then
    assume x = 4
  else
    throw
  end
catch
  assert y + x = 5
endc