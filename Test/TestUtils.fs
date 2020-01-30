module Test.Utils

open Microsoft.VisualStudio.TestTools.UnitTesting
open ResourceManager
open QuantumConcepts.Formats.StereoLithography

/// <summary>
///  Create a data array for MSTest data-driven tests from a list of tuples.
/// </summary>
/// <remarks>
///  <para>
///   This is so we can have computed properties in the data array, or share data
///   array across tests. DataRow attribute (and attributes in general) don't
///   allow computed properties (such as 2 ** 2, Array.empty, etc.) to appear in
///   them. This is too limiting for what we're trying to do.
///  </para>
///  <para>
///   Using tuples instead of lists of lists allows us to ensure the same type
///   is put in every element of the tuple, and that each tuple has the exact
///   same number of elements.
///  </para>
/// </remarks>
/// <example>
///  Usage (note extra parentheses in the test method):
///  <code>
///   static member testData = dataArray [ arg1, arg2; ... ]
///   [&lt;TestMethod>]
///   [&lt;DynamicData("testData")>]
///   member this.Test((el1, el2)) =
///  </code>
/// </example>
let dataArray listOfTuples : System.Object[] list =
    listOfTuples |> List.map (fun tuple -> [| tuple |])

type Assert with
    /// <summary>
    ///  Type-checked and curried version of Assert.AreEqual - types have to be
    ///  the same at compile time
    /// </summary>
    /// <remarks>
    ///  Regular Asssert.AreEqual has an overload that boxes both arguments in
    ///  case of type mismatch which makes tests harder to write and messages -
    ///  useless (dumps internal assembly information on an object rather than
    ///  "wrong type" information).
    ///  As a trade off, this adds one additional step to the stack trace, so
    ///  always click the bottom test.
    ///  This also works with F#'s type system - now you can use List.empty and
    ///  rely on type deduction.
    /// </remarks>
    static member equal (expected : 'u) (actual : 'u) =
        Assert.AreEqual<'u>(expected, actual)

    /// <summary>
    ///  Assert.equal with a comment
    /// </summary>
    /// <seealso cref="Assert.equal"/>
    static member equal2 (expected : 'u) (message : string) (actual : 'u) =
        Assert.AreEqual<'u>(expected, actual, message)

    /// <summary>
    ///  Type-checked and curried version of Assert.AreSame - types have to be
    ///  the same compile-time
    /// </summary>
    /// <seealso cref="Assert.equal"/>
    static member same (expected : 'u) (actual : 'u) =
        Assert.AreSame(expected, actual)

    /// <summary>
    ///  Assert.same with a comment
    /// </summary>
    /// <seealso cref="Assert.same"/>
    static member same2 (expected : 'u) (message : string) (actual : 'u) =
        Assert.AreSame(expected, actual, message)

// Version of resource manager that doesn't depend on files and returns null
// resources. For use in testing
type ResourceManagerStub() =
    interface IResourceManager with
        override this.Texture(name : string) = Unchecked.defaultof<Texture2D>
        override this.Mesh(name : string) = new STLDocument()
