class ClassB
{
    public static string Serialize(object value)
    {
        return Newtonsoft.Json.JsonConvert.SerializeObject(value);
    }
}
