assume x <= 50;
assume y < x;
if x < 100 then
  assert y < 100;
  x := x + 1;
  y := y + 1;
  if x < 100 then 
    assert y < 100;
    x := x + 1;
    y := y + 1
  else
    skip
  end
else
  skip
end