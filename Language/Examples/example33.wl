try
  if x = 1 then
    assume y = 4
  else
    throw
  end;
  assert y = 3
catch
  assert y + x = 5
endc