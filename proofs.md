## 1.13
```
φ = phi = (1 + sqrt(5))/2 = phi^2 - 1
ψ = psi = (1 - sqrt(5))/2 = psi^2 - 1

prove

fib(n) = (phi^n - psi^n)/sqrt(5)


base case
fib(0) = (phi^0 - psi^0)/sqrt(5) = 0
fib(1) = (phi^1 - psi^1)/sqrt(5) = 1

assume
fib(n-1) = (phi^(n-1) - psi^(n-1))/sqrt(5)
fib(n-2) = (phi^(n-2) - psi^(n-2))/sqrt(5)

induction

fib(n) = fib(n-1) + fib(n-2)
       = (phi^(n-1) - psi^(n-1))/sqrt(5) - (phi^(n-2) - psi^(n-2))/sqrt(5)
       = (phi^(n-1) + phi^(n-2) - psi^(n-1) - psi^(n-2) )/sqrt(5)
       = (phi^(n-2)(phi+1) - psi^(n-1)(psi+1))/sqrt(5)
       = (phi^(n-2)*phi^2 - psi^(n-1)*psi^2)/sqrt(5)
       = (phi^n - psi^n)/sqrt(5)
```

## 2.9
```
let interval x = (a, b)
    interval y = (c, d)

then width(x) = (b - a)/2
     width(y) = (d - c)/2
     x+y = (a+c, b+d)

width(x+y) = (b+d-a-c)/2 = (b-a)/2 + (d-c)/2 = width(x) + width(y)

but width(x*y) = (b*d-a*c)/2 which is not a function of width(x) or width(y)
```

## 2.13
```
let x = (a-dx, a+dx)
    y = (b-dy, b+dy)

    where dx and dy are small tolerances

z = x . y = (ab-bdx-ady+dxdy, ab+bdx+bdy+dxdy)

if we ignore dxdy based on the small tolerance assumption
the new tolerance = bdx+ady

hense z = (ab - (bdx+ady), ab+(bdx+ady))
```
