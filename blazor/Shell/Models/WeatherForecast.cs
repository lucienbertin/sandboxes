using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Shell.Models;
public class WeatherForecast
{
    public DateOnly Date { get; set; }
    public int TemperatureC { get; set; }
    public string? Summary { get; set; }

    [Column(TypeName = "decimal(18, 2)")]
    public int TemperatureF => 32 + (int)(TemperatureC / 0.5556);
}