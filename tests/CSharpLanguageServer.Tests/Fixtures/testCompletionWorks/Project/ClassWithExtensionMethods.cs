class ClassWithExtensionMethods
{
    public void MethodA(string arg)
    {
        this.
    }
}

public static class ClassExtensions
{
    public static string MethodB(this ClassWithExtensionMethods input)
    {
        return "ok";
    }
}
