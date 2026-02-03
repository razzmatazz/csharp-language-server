using System;

namespace Project.ClassAndInterfaceHierarchy;

interface IGreetable
{
    string GetGreeting();
}

class Person : IGreetable
{
    public string Name { get; set; }

    public Person(string name)
    {
        Name = name;
    }

    public virtual string GetGreeting()
    {
        return $"Hello, my name is {Name}.";
    }
}

class Student : Person
{
    public int Grade { get; set; }

    public Student(string name, int grade) : base(name)
    {
        Grade = grade;
    }

    public override string GetGreeting()
    {
        return $"Hi, I'm {Name} and I'm in grade {Grade}.";
    }
}
