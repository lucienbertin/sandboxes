namespace test;
using blazor.Components.Pages;

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
}
