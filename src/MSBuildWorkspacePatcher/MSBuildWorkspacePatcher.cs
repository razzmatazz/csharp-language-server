using System.Text;
using HarmonyLib;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.MSBuild;

namespace CSharpLanguageServer.MSBuildWorkspacePatcher;

// A very hack way to prevent MSBuildWorkspace.TryApplyChanges from saving the changes to disk.
//
// To avoid saving Solution obj every time (every change will create a new Solution obj), we use
// MSBuildWorkspace in WorkspaceManager. However, MSBuildWorkspace.TryApplyChanges will write all changes to
// disk, which is not we want for a language server. I have tried the following ways, but all fails:
// 1. Inherit MSBuildWorkspace and override all callers of MSBuildWorkspace.TryApplyChanges: MSBuildWorkspace
//    is sealed so it can't be inherited.
// 2. Use Castle.Core to generate a proxy and avoid calling MSBuildWorkspace.TryApplyChanges: MSBuildWorkspace
//    hasn't default constructor so Castle.Core will throw a System.NotSupportedException.
// 3. Inherit WorkSpace and implement OpenSolutionAsync & OpenProjectAsync ourself: The implementations of
//    MSBuildWorkspace.OpenSolutionAsync & MSBuildWorkspace.OpenProjectAsync use many `internal` methods. If
//    we rewrite them all, then we will rewrite the whole Microsoft.CodeAnalysis &
//    Microsoft.CodeAnalysis.MSBuild, which is a huge work and is not suitable for a language server
//    implementation.
// For now, this is the only way I find that can work. But it also has some limitations:
// 1. Harmony doesn't support all .Net version. If we want to upgrade .Net version, we need to wait Harmony
//    releases a version that support the .Net version we want to upgrade to.
[HarmonyPatch(typeof(MSBuildWorkspace))]
[HarmonyPatch("SaveDocumentText")]
class MSBuildWorkspaceSkipSaveDocumentText
{
    static bool Prefix(DocumentId id, string fullPath, SourceText newText, Encoding encoding)
    {
        // false means "don't call the original method"
        return false;
    }
}

public class MSBuildWorkspacePatcher
{
    // This method should be called at the start of the program!
    public static void Patch()
    {
        var harmony = new Harmony("MSBuildWorkspacePatcher");
        harmony.PatchAll();
    }
}
