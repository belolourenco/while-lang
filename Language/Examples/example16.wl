assume z = x;
z := 1;
while x <= 0 do {(x > y -1) && (y >= x * z)}
  x := y - 1;
  y := x * z
od;
assert z = y