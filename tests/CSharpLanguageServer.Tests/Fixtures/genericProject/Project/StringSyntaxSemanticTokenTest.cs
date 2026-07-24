using System.Diagnostics.CodeAnalysis;

namespace Project;

internal static class StringSyntaxSemanticTokenTest
{
    private static void AcceptRegex([StringSyntax(StringSyntaxAttribute.Regex)] string pattern) { }

    private static void AcceptJson([StringSyntax(StringSyntaxAttribute.Json)] string document) { }

    public static void EmbeddedLanguages()
    {
        AcceptRegex(@"$(\a\\)|[^\p{Lu}-a\w\sa-z-[m-p]]+?(?#comment)|(?<name>sub){0,5}?^");
        AcceptJson(@"[/*comment*/{ 'goo': 0, bar: -Infinity, ""baz"": true }, new Date(), text, 'str'] // comment");
    }
}
