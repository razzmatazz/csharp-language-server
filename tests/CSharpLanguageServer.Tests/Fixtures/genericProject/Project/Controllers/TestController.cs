using Microsoft.AspNetCore.Mvc;
using Project.Models.Test;

namespace Printlog.Web.ClientPart.Controllers;

public class TestController : Controller
{
    public IActionResult Index()
    {
        var model = new IndexViewModel()
        {
            Output = "test"
        };

        return View(model);
    }
}
