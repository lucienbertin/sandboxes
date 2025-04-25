namespace Domain.Weather

type Cloudy = 
| ABit
| ALot
| ReallyCloudy

type Summary = 
| Sunny
| Cloudy of Cloudy
| Raining of int

type Forecast = {
    Id: int
    TemperatureC : int
    Summary : Summary
    Date : System.DateOnly
}