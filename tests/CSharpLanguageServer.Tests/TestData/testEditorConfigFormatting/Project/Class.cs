// csharp_space_after_keywords_in_control_flow_statements = false
if (true) ;

// csharp_space_before_open_square_brackets = true
var array = new int[10];

// csharp_space_after_cast = true
var num = (int)15.0;

// csharp_new_line_before_open_brace = none
void foo()
{

}

// csharp_indent_switch_labels = true
switch (Random.Shared.Next(3))
{
    case 1:
        Console.WriteLine("Case 1");
        break;
    case 2:
        Console.WriteLine("Case 2");
        break;
    default:
        Console.WriteLine("Default case");
        break;
}

