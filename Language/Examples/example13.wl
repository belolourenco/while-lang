assume x <= 10;
while x <= 0 do
  x := y - 1;
  y := x * z
od;
y := x + 1;
w := x + y + z;
assert y + x = w