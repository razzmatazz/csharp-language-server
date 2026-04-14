// IDE0040: missing 'private' on field
// IDE0051: unused private method
// IDE0032: backing field could be auto property
class MyClass
{
    int _value = 0;                 // IDE0040 (no accessibility modifier)

    int Value { get { return _value; } set { _value = value; } }  // IDE0032

    void UnusedMethod() { }        // IDE0051
}
