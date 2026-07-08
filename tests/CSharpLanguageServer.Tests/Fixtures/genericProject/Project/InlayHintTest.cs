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

    public delegate void MessageDispatcherAsync();

    public class ResourceCondition
    {
    }

    public class IdentifierEchoesTypeSubject
    {
        private static ResourceCondition GetCondition()
        {
            return new ResourceCondition();
        }

        private static System.Collections.Generic.List<Widget> GetWidgets()
        {
            return new System.Collections.Generic.List<Widget>();
        }

        private static void Dispatch(MessageDispatcherAsync messageDispatcherAsync)
        {
        }

        private static void DispatchWithGenericName(MessageDispatcherAsync handler)
        {
        }

        public void FullNameMatchTypeHintIsSuppressed()
        {
            var resourceCondition = GetCondition();
        }

        public void LastWordMatchTypeHintIsSuppressed()
        {
            var settingChangeCondition = GetCondition();
        }

        public void NonMatchingIdentifierKeepsTypeHint()
        {
            var outcome = GetCondition();
        }

        public void ForeachVariableEchoingElementTypeIsSuppressed()
        {
            foreach (var widget in GetWidgets())
            {
            }
        }

        public void ForeachVariableNotEchoingElementTypeKeepsHint()
        {
            foreach (var item in GetWidgets())
            {
            }
        }

        public void ParameterNameEchoingOwnTypeIsSuppressed()
        {
            Dispatch(null);
        }

        public void ParameterNameNotEchoingOwnTypeKeepsHint()
        {
            DispatchWithGenericName(null);
        }
    }

    public class GenericParameterNameHelper
    {
        public void Save(object obj)
        {
        }

        public void Log(string msg)
        {
        }
    }

    public class GenericParameterNameSubject
    {
        private readonly GenericParameterNameHelper helper = new GenericParameterNameHelper();

        public void GenericObjParameterNameIsSuppressed()
        {
            helper.Save(5);
        }

        public void NonGenericSameLengthParameterNameKeepsHint()
        {
            helper.Log("hi");
        }
    }

    public class WidgetWithProperty
    {
        public int Value { get; set; }
    }

    public class ExplicitObjectCreationTypeHintSubject
    {
        public void ExplicitObjectCreationTypeHintIsSuppressed()
        {
            var widget = new Widget();
        }

        public void ExplicitObjectCreationWithInitializerTypeHintIsSuppressed()
        {
            var widget = new WidgetWithProperty { Value = 1 };
        }
    }

    public class ValueParameterHelper
    {
        public bool Contains(int value)
        {
            return value > 0;
        }

        public void Add(int key, int value)
        {
        }
    }

    public class ValueParameterSubject
    {
        private readonly ValueParameterHelper helper = new ValueParameterHelper();

        public void SoleValueParameterNameIsSuppressed()
        {
            helper.Contains(5);
        }

        public void MultiArgumentValueParameterNameKeepsHint()
        {
            helper.Add(1, 2);
        }
    }

    public class StaticQualifierTypeHintSubject
    {
        public void StaticQualifierMatchingReturnTypeIsSuppressed()
        {
            var reason = string.Format("{0}", 1);
        }

        public void StaticQualifierNotMatchingReturnTypeKeepsHint()
        {
            var converted = System.Convert.ToInt32("5");
        }
    }

    public class ItemParameterHelper
    {
        public void Add(int item)
        {
        }

        public void Insert(int index, int item)
        {
        }
    }

    public class ItemParameterSubject
    {
        private readonly ItemParameterHelper helper = new ItemParameterHelper();

        public void SoleItemParameterNameIsSuppressed()
        {
            helper.Add(5);
        }

        public void MultiArgumentItemParameterNameKeepsHint()
        {
            helper.Insert(0, 5);
        }
    }
}
