namespace test;
using app.Components.Pages;
using domain;

public class UnitTest1
{
    [Fact]
    public void ShouldPass()
    {
        Assert.Equal(8, 8);
    }
    // [Fact]
    // public void ShouldFail()
    // {
    //     Assert.Equal(8, 12);
    // }

    [Fact]
    public void CounterShouldIncrementWhenClicked()
    {
        // Arrange
        using var ctx = new TestContext();
        var cut = ctx.RenderComponent<Counter>();
        var paraElm = cut.Find("p");

        // Act
        cut.Find("button").Click();
        cut.Find("button").Click();
        cut.Find("button").Click();

        // Assert
        var paraElmText = paraElm.TextContent;
        paraElmText.MarkupMatches("N: 4");
    }

    [Fact]
    public void Counter()
    {
        // Arrange
        Counter c = new();

        // Act
        c.Increment();

        // Assert
        Assert.Equal(2, c.n);
    }

    [Fact]
    public void Increment()
    {
        // Arrange
        int n = 4;

        // Act
        int p = Incr.incr(n);

        // Assert
        Assert.Equal(5, p);
    }

    [Fact]
    public void Fibonacci()
    {
        // Arrange
        int n = 30;

        // Act
        int fib = Incr.fibonacci(n);

        // Assert
        Assert.Equal(832040, fib);
    }
}
