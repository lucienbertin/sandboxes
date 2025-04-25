using Domain.Weather;
// using Microsoft.FSharp.Core;

namespace Shell.Db;
public class WeatherForecast
{
    public int WeatherForecastId { get; set; }
    public DateOnly Date { get; set; }
    public int TemperatureC { get; set; }
    public string? Summary { get; set; }
}

public class WeatherForecastRepository(ShellDbContext context)
{
    private readonly ShellDbContext context = context;

    public Forecast GetById(int id) {
        var dbForecast = context.WeatherForecasts.Where(f => f.WeatherForecastId == id).First();
        return ToForecast(dbForecast);
    }

    private static Forecast ToForecast(WeatherForecast wf) {
        Summary summary = Summary.NewCloudy(Cloudy.ABit);
        return new Forecast(
            wf.WeatherForecastId,
            wf.TemperatureC,
            // wf.Summary == null ? FSharpOption<string>.None : FSharpOption<string>.Some(wf.Summary),
            summary,
            wf.Date
        );
    }

}
