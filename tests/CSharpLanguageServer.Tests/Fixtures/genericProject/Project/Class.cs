using System;

class Class
{
    public void MethodA(string arg)
    {
        string str = "";
        Console.WriteLine(str);
    }

    public void MethodB(string arg)
    {
        MethodA(arg);
    }
}
