using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Shell.Models;

namespace Shell;

public partial class BlazorDbContext : DbContext
{
    public DbSet<WeatherForecast> WeatherForecasts { get; set; }
    public BlazorDbContext()
    {
    }

    public BlazorDbContext(DbContextOptions<BlazorDbContext> options)
        : base(options)
    {
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
    }
    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        OnModelCreatingPartial(modelBuilder);
    }

    partial void OnModelCreatingPartial(ModelBuilder modelBuilder);
}
