namespace Domain

module Incr = 
    let incr (n: int): int =
        n+1

module Fib =
    let rec fibonacci(n: int):int = 
        match n with
        | 0 | 1 -> n
        | n -> fibonacci (n-1) + fibonacci (n - 2)