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
                    // Deliberate CS8600: assigning null to a non-nullable string triggers
                    // a warning in nullable-enabled projects, so workspace/diagnostic
                    // would surface it for the generated file — which users cannot fix.
                    public static void Dummy() { string s = null; _ = s; }
                }
                """, Encoding.UTF8));
        });
    }
}
