using Newtonsoft.Json;

public class MyClass
{
    public string ToJson(object o)
    {
        return JsonConvert.SerializeObject(o);
    }
}
