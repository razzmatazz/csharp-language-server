using System;
using System.Collections.Generic;

namespace Project.FoldingRangeTest
{
    /// <summary>
    /// A type used to test folding range support.
    /// </summary>
    public class FoldingSubject
    {
        #region Fields

        private int _value;
        private string _name;

        #endregion

        public FoldingSubject(int value, string name)
        {
            _value = value;
            _name = name;
        }

        public int Value
        {
            get { return _value; }
            set { _value = value; }
        }

        public string Greet()
        {
            /* This is a
               multi-line comment */
            return $"Hello, {_name}!";
        }

        public void MultiMethod()
        {
            Console.WriteLine("a");
            Console.WriteLine("b");
        }
    }

    public interface IFoldable
    {
        string GetName();
        int GetValue();
    }
}
