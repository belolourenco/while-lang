y := e1;
assert theta0 = 0;
if b = 12 then
  x := e2;
  assert theta1 = 1
else
  assert theta2 = 2
end;
assert theta3 = 3