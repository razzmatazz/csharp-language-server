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
}
