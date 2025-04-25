using Domain;
using Microsoft.FSharp.Core;

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
    public Weather.Forecast getById(int id) {
        var dbForecast = context.WeatherForecasts.Where(f => f.WeatherForecastId == id).First();
        return toForecast(dbForecast);
    }

    private static Weather.Forecast toForecast(WeatherForecast wf) {
        return new Weather.Forecast(
            wf.WeatherForecastId,
            wf.TemperatureC,
            wf.Summary == null ? FSharpOption<string>.None : FSharpOption<string>.Some(wf.Summary),
            wf.Date
        );
    }

}
