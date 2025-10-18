<<<<<<<< HEAD:tests/CSharpLanguageServer.Tests/Fixtures/genericProject/Project/ClassForCompletionTestsWithExtensionMethods.cs
class ClassForCompletionWithExtensionMethods
========
class ClassWithExtensionMethods
>>>>>>>> 902dbd5 (squash):tests/CSharpLanguageServer.Tests/Fixtures/genericProject/Project/ClassWithExtensionMethods.cs
{
    public void MethodA(string arg)
    {
        this.
    }
}

public static class ClassExtensions
{
<<<<<<<< HEAD:tests/CSharpLanguageServer.Tests/Fixtures/genericProject/Project/ClassForCompletionTestsWithExtensionMethods.cs
    public static string MethodB(this ClassForCompletionWithExtensionMethods input)
========
    public static string MethodB(this ClassWithExtensionMethods input)
>>>>>>>> 902dbd5 (squash):tests/CSharpLanguageServer.Tests/Fixtures/genericProject/Project/ClassWithExtensionMethods.cs
    {
        return "ok";
    }
}
