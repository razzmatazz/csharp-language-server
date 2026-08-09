using System;
using Newtonsoft.Json;

class Class
{
    public void MethodA(string arg)
    {
        string str = JsonConvert.SerializeObject(arg);
        Console.WriteLine(str);
    }
}
