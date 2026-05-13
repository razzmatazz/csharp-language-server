namespace ProjectB;

public class SomeClass
{
    // CS0103: "The name 'the_42_B' does not exist in the current context"
    // The substring "42_B" appears verbatim in the diagnostic message.
    static object _ = the_42_B;
}
