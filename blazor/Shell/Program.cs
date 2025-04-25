using Microsoft.EntityFrameworkCore;
using Shell.Db;
using Shell.Components;

var builder = WebApplication.CreateBuilder(args);

var conString = builder.Configuration.GetConnectionString("PostgresConnection") ??
     throw new InvalidOperationException("Connection string 'PostgresConnection' not found.");

builder.Services.AddDbContextFactory<ShellDbContext>(options =>
    options.UseNpgsql(conString));

// Add services to the container.
builder.Services.AddRazorComponents()
    .AddInteractiveServerComponents();

var application = builder.Build();

// Configure the HTTP request pipeline.
if (!application.Environment.IsDevelopment())
{
    application.UseExceptionHandler("/Error", createScopeForErrors: true);
    // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
    application.UseHsts();
}

application.UseHttpsRedirection();


application.UseAntiforgery();

application.MapStaticAssets();
application.MapRazorComponents<App>()
    .AddInteractiveServerRenderMode();

application.Run();
