using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using System.Text;

namespace TestGenerator;

[Generator]
public class HelloGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        context.RegisterPostInitializationOutput(ctx =>
        {
            ctx.AddSource("Generated.g.cs", SourceText.From("""
                namespace Generated;
                public static class Hello
                {
                    public static string World => "hello";
                }
                """, Encoding.UTF8));
        });
    }
}
