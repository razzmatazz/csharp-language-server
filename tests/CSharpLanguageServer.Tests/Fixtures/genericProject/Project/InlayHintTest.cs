namespace Project.InlayHintTest
{
    public class InlayHintHelper
    {
        public void WithResource(string resourceName, string other)
        {
        }

        public int Add(int first, int second)
        {
            return first + second;
        }
    }

    public class InlayHintSubject
    {
        private readonly InlayHintHelper helper = new InlayHintHelper();

        public string ResourceName { get; set; } = "";
        public string OtherValue { get; set; } = "";

        private int GetCount() => 42;

        public void TypeHintOnVarDeclaration()
        {
            var count = GetCount();
        }

        public void ParameterHintOnRegularCall()
        {
            helper.Add(1, 2);
        }

        public void SameNameSingleLineArgumentIsSuppressed()
        {
            string resourceName = "abc";
            helper.WithResource(resourceName, "other");
        }

        public void SameNameMultiLineArgumentIsSuppressed()
        {
            string resourceName = "abc";
            helper.WithResource(
                resourceName,
                "other");
        }

        public void QualifiedMemberAccessSameNameIsSuppressed()
        {
            helper.WithResource(
                this.ResourceName,
                "other");
        }

        public void DifferentNameArgumentKeepsHint()
        {
            string somethingElse = "abc";
            helper.WithResource(somethingElse, "other");
        }

        public void DifferentQualifiedMemberAccessKeepsHint()
        {
            helper.WithResource(
                this.OtherValue,
                "other");
        }
    }

    public class ShortAndNumberedParameterHelper
    {
        public void SetX(int x)
        {
        }

        public void WriteBuffer(byte[] buffer, int offset, int count)
        {
        }
    }

    public class ShortAndNumberedParameterSubject
    {
        private readonly ShortAndNumberedParameterHelper helper = new ShortAndNumberedParameterHelper();

        public void ShortParameterNameIsSuppressed()
        {
            helper.SetX(5);
        }

        public void NumberedSuffixParameterNameIsSuppressed()
        {
            string.Format("{0} {1}", 1, 2);
        }

        public void WellKnownBclNumberedSuffixParameterNamesAreSuppressed()
        {
            System.Math.Min(1, 2);
        }

        public void OpaqueBclShortParameterNamesAreSuppressed()
        {
            System.Math.Round(1.23m, 2, System.MidpointRounding.AwayFromZero);
        }

        public void DisambiguatingParameterNamesKeepHint()
        {
            "hello".Substring(1, 2);
        }

        public void BufferOffsetCountParameterNamesKeepHint()
        {
            var data = new byte[] { 1, 2, 3 };
            helper.WriteBuffer(data, 0, data.Length);
        }
    }

    public class FluentQuery
    {
        public FluentQuery Where(System.Func<int, bool> predicate)
        {
            return this;
        }

        public FluentQuery Fetch(System.Func<int, int> relatedObjectSelector)
        {
            return this;
        }

        public FluentQuery ThenFetch(System.Func<int, int> relatedObjectSelector)
        {
            return this;
        }

        public FluentQuery Combine(System.Func<int, int> first, System.Func<int, int> second)
        {
            return this;
        }
    }

    public class FluentQuerySubject
    {
        private static bool IsPositive(int x) => x > 0;

        public void SingleLambdaArgumentToWhereIsSuppressed()
        {
            new FluentQuery().Where(x => x > 0);
        }

        public void SingleLambdaArgumentToFluentOrmStyleMethodIsSuppressed()
        {
            new FluentQuery().Fetch(x => x).ThenFetch(x => x);
        }

        public void MultiLambdaArgumentCallKeepsHints()
        {
            new FluentQuery().Combine(x => x, x => x);
        }

        public void SingleMethodGroupArgumentKeepsHint()
        {
            new FluentQuery().Where(IsPositive);
        }
    }

    // Mirrors the real log4net `ILog.DebugFormat` overload shapes cited in
    // plans/inlay-hint-reduction.md's rule #4 evidence: explicit `arg0`/`arg1`/`arg2` parameters
    // rather than a single `params object[] args`.
    public class Logger
    {
        public void DebugFormat(string format, object arg0, object arg1, object arg2)
        {
        }
    }

    public class LoggerSubject
    {
        public void CompositeFormatStringPositionalArgumentsAreSuppressed()
        {
            new Logger().DebugFormat("{0}: {1} did {2}", 1, 2, 3);
        }
    }

    // A trailing CancellationToken argument is a conventional, non-essential "pass-through"
    // parameter on async APIs, so it shouldn't disqualify the single-lambda-argument rule (#3).
    public class FluentQueryAsync
    {
        public FluentQueryAsync WhereAsync(
            System.Func<int, bool> predicate,
            System.Threading.CancellationToken cancellationToken)
        {
            return this;
        }

        public FluentQueryAsync CombineAsync(
            System.Func<int, int> first,
            System.Func<int, int> second,
            System.Threading.CancellationToken cancellationToken)
        {
            return this;
        }
    }

    public class FluentQueryAsyncSubject
    {
        public void SingleLambdaArgumentWithTrailingCancellationTokenIsSuppressed()
        {
            new FluentQueryAsync().WhereAsync(x => x > 0, default);
        }

        public void MultiLambdaArgumentWithTrailingCancellationTokenKeepsHints()
        {
            new FluentQueryAsync().CombineAsync(x => x, x => x, default);
        }
    }

    public class Widget
    {
    }

    public static class GenericFactory
    {
        public static T Create<T>() where T : new()
        {
            return new T();
        }

        public static string Describe<T>(T value)
        {
            return value?.ToString() ?? "";
        }
    }

    public class GenericFactorySubject
    {
        private T CreateLocal<T>() where T : new()
        {
            return new T();
        }

        public void QualifiedGenericInvocationSpellingOutTypeIsSuppressed()
        {
            var widget = GenericFactory.Create<Widget>();
        }

        public void QualifiedGenericInvocationWithDifferentReturnTypeKeepsHint()
        {
            var description = GenericFactory.Describe<Widget>(new Widget());
        }

        public void UnqualifiedGenericInvocationSpellingOutTypeIsSuppressed()
        {
            var widget = CreateLocal<Widget>();
        }

        public void RealBclEnumParseGenericInvocationIsSuppressed()
        {
            var day = System.Enum.Parse<System.DayOfWeek>("Monday");
        }
    }
}
