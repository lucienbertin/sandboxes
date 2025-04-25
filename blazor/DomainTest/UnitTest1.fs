module DomainTest

open NUnit.Framework
open Domain

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Incr3 () =
    Assert.That(Incr.incr 3, Is.EqualTo(4))

[<Test>]
let Fib5 () =
    Assert.That(Fib.fibonacci 5, Is.EqualTo(5))

[<Test>]
let Fib17 () =
    Assert.That(Fib.fibonacci 17, Is.EqualTo(1597))
