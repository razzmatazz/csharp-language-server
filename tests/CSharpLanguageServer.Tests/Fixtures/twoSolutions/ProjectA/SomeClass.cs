namespace ProjectA;

public class SomeClass
{
    // CS0103: "The name 'hello_A' does not exist in the current context"
    // The substring "hello_A" appears verbatim in the diagnostic message.
    static object _ = hello_A;
}
