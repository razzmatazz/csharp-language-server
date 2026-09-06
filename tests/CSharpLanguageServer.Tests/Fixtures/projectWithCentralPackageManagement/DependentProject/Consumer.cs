public class Consumer
{
    public string Convert(object o)
    {
        var myClass = new MyClass();
        return myClass.ToJson(o);
    }
}
