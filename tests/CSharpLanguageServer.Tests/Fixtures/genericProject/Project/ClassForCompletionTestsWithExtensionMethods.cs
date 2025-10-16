class ClassForCompletionWithExtensionMethods
{
    public void MethodA(string arg)
    {
        this.
    }
}

public static class ClassExtensions
{
    public static string MethodB(this ClassForCompletionWithExtensionMethods input)
    {
        return "ok";
    }
}
