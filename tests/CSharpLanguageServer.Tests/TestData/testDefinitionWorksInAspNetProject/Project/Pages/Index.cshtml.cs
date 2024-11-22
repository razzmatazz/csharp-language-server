using Microsoft.AspNetCore.Mvc.RazorPages;
namespace test_csharp_web.Pages;
public class IndexModel : PageModel
{
    public string? Value { get; set; }
    public void OnGet()
    {
        Value = "test";
    }
}
