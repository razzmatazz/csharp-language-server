using System;

namespace Project
{
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

    class Program
    {
        static void Main()
        {
            string normal = "C# is fun!";
            string verbatim = @"Path with \slashes\ stays literal";
            string interpolated = $"Today is {DateTime.Now:dddd}";

            Console.WriteLine(normal);

            IGreetable p1 = new Person("Alice");
            IGreetable p2 = new Student("Bob", 10);

            Console.WriteLine(p1.GetGreeting());

            void Shout(string msg) => Console.WriteLine(msg.ToUpper());
            Shout("this is a local function!");

            var a = $"""
test for
multiline token
""";
        }
    }
}
