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
