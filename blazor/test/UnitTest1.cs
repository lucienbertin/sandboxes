namespace test;

public class UnitTest1
{
    [Fact]
    public void ShouldPass()
    {
        Assert.Equal(8, 8);
    }
    [Fact]
    public void ShouldFail()
    {
        Assert.Equal(8, 8);
    }
}
