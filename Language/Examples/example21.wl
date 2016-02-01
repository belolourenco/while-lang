if x0 > 0 then
  y1 := 1
else 
  y1 := 0
end;
assert (y1 = 0) || (y1 = 1);
if x0 > 0 then 
  y2 := 1 
else 
  y2 := 0
end;
assert y2 = y1;
if x0>0 then
  y3 := 1
else 
  y3 := 0
end;
assert y3 = y1