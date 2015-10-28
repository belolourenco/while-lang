x := y;
if x <= 0 then
  x := z + 1;
  z := 2
else
  z := x;
  y := w + 2
end
p := x + z + y