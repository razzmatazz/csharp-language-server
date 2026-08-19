using System;

class OutgoingComplex
{
    public void Orchestrator()
    {
        var w = new Widget();
        Func<int, int> f = x => Helper(x);
        LocalHelper();
        f(2);
        w.Render();

        void LocalHelper()
        {
            Helper(7);
        }
    }

    public int Helper(int x) => x;
}

class Widget
{
    public Widget()
    {
    }

    public void Render()
    {
    }
}
