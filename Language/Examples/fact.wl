assume (n >= 0);
f := 1;
i := 1;
while i <= n do {(f = (i - 1)) ^ (i <= n + 1) ^ (aux = n)}
  f := f * i ;
  i := i + 1
od;
assert (f = n)