using Microsoft.AspNetCore;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting;

namespace Project;

public class Program
{
    public static async Task Main(string[] args)
    {
        await BuildWebHost(args).RunAsync();
    }

    public static IWebHost BuildWebHost(string[] args)
    {
        var builder = WebHost.CreateDefaultBuilder(args);
        builder = builder.UseKestrel().UseStartup<Startup>();
        return builder.Build();
    }
}
