using Domain;
using Microsoft.FSharp.Core;


namespace Shell.Models;
public class WeatherForecast
{
    public int WeatherForecastId { get; set; }
    public DateOnly Date { get; set; }
    public int TemperatureC { get; set; }
    public string? Summary { get; set; }

    public Weather.Forecast toForecast() {
        return new Weather.Forecast(
            WeatherForecastId,
            TemperatureC,
            Summary == null ? FSharpOption<string>.None : FSharpOption<string>.Some(Summary),
            Date
        );
    }
}