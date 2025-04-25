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

public class WeatherForecastRepository {
    private ShellDbContext context;

    public WeatherForecastRepository(ShellDbContext context)
    {
        this.context = context;
    }
    public Forecast getById(int id) {
        var dbForecast = context.WeatherForecasts.Where(f => f.WeatherForecastId == id).First();
        return toForecast(dbForecast);
    }

    private static Forecast toForecast(WeatherForecast wf) {
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
