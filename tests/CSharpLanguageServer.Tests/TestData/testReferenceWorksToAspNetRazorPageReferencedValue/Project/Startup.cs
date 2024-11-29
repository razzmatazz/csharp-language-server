using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;

namespace Project;

public class Startup
{
    public Startup(IConfiguration configuration, IWebHostEnvironment env)
    {
    }

    public void ConfigureServices(IServiceCollection services)
    {
        services.AddOptions();
    }

    public void Configure(
        IApplicationBuilder app,
        IWebHostEnvironment env)
    {
        app.UseAuthentication();
        app.UseAuthorization();

        app.UseEndpoints(endpoints => {
                endpoints.MapControllers();
            });
    }
}
