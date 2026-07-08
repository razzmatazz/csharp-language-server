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
}
