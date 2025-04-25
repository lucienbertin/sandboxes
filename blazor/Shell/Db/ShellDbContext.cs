using Microsoft.EntityFrameworkCore;

namespace Shell.Db;

public partial class ShellDbContext : DbContext
{
    public DbSet<WeatherForecast> WeatherForecasts { get; set; }
    public ShellDbContext()
    {
    }

    public ShellDbContext(DbContextOptions<ShellDbContext> options)
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
