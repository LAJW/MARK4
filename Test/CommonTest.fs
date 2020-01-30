namespace Test.Common

open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Test.Utils

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

[<TestClass>]
type DictionaryAddOrAssign() =
    [<TestMethod>]
    member this.IntoEmpty() =
        let dict = Dictionary<int, int>()
        dict.AddOrAssign(3, 7)
        Assert.AreEqual(1, dict.Count)
        Assert.AreEqual(7, dict.[3])

    [<TestMethod>]
    member this.IntoPreexisting() =
        let dict = Dictionary<int, int>()
        dict.Add(3, 7)
        dict.Add(4, 9)
        dict.Add(5, 11)
        dict.AddOrAssign(6, 13)
        Assert.AreEqual(4, dict.Count)
        Assert.AreEqual(7, dict.[3])
        Assert.AreEqual(9, dict.[4])
        Assert.AreEqual(11, dict.[5])
        Assert.AreEqual(13, dict.[6])

    [<TestMethod>]
    member this.Override() =
        let dict = Dictionary<int, int>()
        dict.AddOrAssign(3, 7)
        dict.AddOrAssign(3, 5)
        Assert.AreEqual(1, dict.Count)
        Assert.AreEqual(5, dict.[3])

[<TestClass>]
type DictionaryTryFind() =
    [<TestMethod>]
    member this.IntoPreexisting() =
        let dict = Dictionary<int, int>()
        dict.Add(3, 7)
        dict.Add(4, 9)
        dict.Add(5, 11)
        Assert.AreEqual(Some 7, dict |> Dictionary.tryFind 3)
        Assert.AreEqual(Some 9, dict |> Dictionary.tryFind 4)
        Assert.AreEqual(Some 11, dict |> Dictionary.tryFind 5)
        Assert.AreEqual(None, dict |> Dictionary.tryFind 6)

[<TestClass>]
type DictionaryToSeq() =
    [<TestMethod>]
    member this.IntoPreexisting() =
        let dict = Dictionary<int, int>()
        dict.Add(3, 7)
        dict.Add(4, 9)
        dict.Add(5, 11)
        let map = dict |> Dictionary.toSeq |> Map
        Assert.AreEqual(3, map.Count)
        Assert.AreEqual(7, map.[3])
        Assert.AreEqual(9, map.[4])
        Assert.AreEqual(11, map.[5])

[<TestClass>]
type Round2() =
    [<TestMethod>]
    member this.round0() =
        Assert.AreEqual(5., round2 0 5.34798)
    [<TestMethod>]
    member this.round0Curry() =
        Assert.AreEqual(5., 5.34798 |> round2 0)
    [<TestMethod>]
    member this.round2() =
        Assert.AreEqual(5.35, round2 2 5.34798)
    [<TestMethod>]
    member this.round2Curry() =
        Assert.AreEqual(5.35, 5.34798 |> round2 2)

    [<TestMethod>]
    member this.round2WithUnits() =
        Assert.AreEqual(5.35<m>, 5.34798<m> |> round2 2)

[<TestClass>]
type Confine() =
    [<DataRow(0,  0, 0, 0)>]
    [<DataRow(3,  1, 5, 3)>]
    [<DataRow(-3, 1, 5, 1)>]
    [<DataRow(10, 1, 5, 5)>]
    [<DataTestMethod>]
    member this.confineInt(value : int, lower : int, upper : int, expected : int) =
        Assert.AreEqual(expected, value |> confine lower upper)

    [<DataRow(0.,  0., 0., 0.)>]
    [<DataRow(3.,  1., 5., 3.)>]
    [<DataRow(-3., 1., 5., 1.)>]
    [<DataRow(10., 1., 5., 5.)>]
    [<DataTestMethod>]
    member this.confineFloat(value : float, lower : float, upper : float, expected : float) =
        Assert.AreEqual(expected, value |> confine lower upper)

    [<DataRow(0.<m>,  0.<m>, 0.<m>, 0.<m>)>]
    [<DataRow(3.<m>,  1.<m>, 5.<m>, 3.<m>)>]
    [<DataRow(-3.<m>, 1.<m>, 5.<m>, 1.<m>)>]
    [<DataRow(10.<m>, 1.<m>, 5.<m>, 5.<m>)>]
    [<DataTestMethod>]
    member this.confineMeters(value : float<m>, lower : float<m>, upper : float<m>, expected : float<m>) =
        Assert.AreEqual(expected, value |> confine lower upper)

    [<TestMethod>]
    member this.confineInvalid() =
        Assert.ThrowsException<System.ArgumentException>(
            (fun _ -> 0 |> confine 5 2 |> ignore)) |> ignore
        Assert.ThrowsException<System.ArgumentException>(
            (fun _ -> 0. |> confine 5. 2. |> ignore)) |> ignore
        Assert.ThrowsException<System.ArgumentException>(
            (fun _ -> 0.<m> |> confine 5.<m> 2.<m> |> ignore)) |> ignore

[<TestClass>]
type Pow2() =
    [<DataRow(0, 0)>]
    [<DataRow(1, 1)>]
    [<DataRow(2, 4)>]
    [<DataRow(3, 9)>]
    [<DataRow(-4, 16)>]
    [<DataTestMethod>]
    member this.intNoUnits(input : int, output : int) =
        Assert.AreEqual(output, input |> pow2)

    [<DataRow(0., 0.)>]
    [<DataRow(1., 1.)>]
    [<DataRow(2., 4.)>]
    [<DataRow(3., 9.)>]
    [<DataRow(-4., 16.)>]
    [<DataTestMethod>]
    member this.floatNoUnits(input, output) =
        Assert.AreEqual(output, input |> pow2, delta)

    [<DataRow(0.<m>, 0.<m^2>)>]
    [<DataRow(1.<m>, 1.<m^2>)>]
    [<DataRow(2.<m>, 4.<m^2>)>]
    [<DataRow(3.<m>, 9.<m^2>)>]
    [<DataRow(-4.<m>, 16.<m^2>)>]
    [<DataTestMethod>]
    member this.floatWithUnits(input, output) =
        Assert.AreEqual(output, input |> pow2, delta)

[<TestClass>]
type ToRad() =
    [<TestMethod>]
    member this.zero() =
        Assert.AreEqual(0., 0. |> toRad |> float, delta)

    [<DataRow(6., 30.)>]
    [<DataRow(4., 45.)>]
    [<DataRow(2., 90.)>]
    [<DataRow(1., 180.)>]
    [<DataRow(0.5, 360.)>]
    [<DataRow(-6., -30.)>]
    [<DataTestMethod>]
    member this.all(input, divider) =
        Assert.AreEqual(Pi / divider, input |> toRad, delta)

[<TestClass>]
type AllOrNothing() =
    [<TestMethod>]
    member this.notEmpty() =
        Assert.AreEqual(
            Some [1; 2; 3],
            [Some 1; Some 2; Some 3] |> Seq.allOrNothing)
    [<TestMethod>]
    member this.empty() =
        Assert.AreEqual(
            None,
            [Some 1; Some 2; None] |> Seq.allOrNothing)

[<TestClass>]
type TryMinBy() =
    let ints = [ -5; -3; 4; 6 ]
    let vecs = [
        vec(1.0, 5.0);
        vec(2.0, 2.0);
        vec(-3.0, -4.0);
    ]
    [<TestMethod>]
    member this.nonEmptyInts() =
        Assert.AreEqual(Some -3, ints |> Seq.tryMinBy abs)
    [<TestMethod>]
    member this.emptyInts() =
        Assert.AreEqual(None, [] |> Seq.tryMinBy abs)
    [<TestMethod>]
    member this.nonEmptyVec1() =
        match vecs |> Seq.tryMinBy Vec.length with
        | Some result ->
            Assert.AreEqual(2.0, result.x, delta)
            Assert.AreEqual(2.0, result.y, delta)
        | None ->
            Assert.IsTrue(false, "tryMinBy should not return None")
    [<TestMethod>]
    member this.nonEmptyVec2() =
        match vecs |> Seq.tryMinBy (fun x -> x - vec(-1.0, -5.0) |> Vec.length) with
        | Some result ->
            Assert.AreEqual(-3.0, result.x, delta)
            Assert.AreEqual(-4.0, result.y, delta)
        | None ->
            Assert.IsTrue(false, "tryMinBy should not return None")
    [<TestMethod>]
    member this.emptyVec() =
        Assert.AreEqual([] |> Seq.tryMinBy Vec.length, None)

[<TestClass>]
type ListCdr() =
    [<TestMethod>]
    member this.empty() =
        Assert.AreEqual([], [] |> List.cdr)
    [<TestMethod>]
    member this.one() =
        Assert.AreEqual(List<int>.Empty, [1] |> List.cdr)
    [<TestMethod>]
    member this.two() =
        Assert.AreEqual([2], [1; 2] |> List.cdr)
    [<TestMethod>]
    member this.three() =
        Assert.AreEqual([2; 3], [1; 2; 3] |> List.cdr)

[<TestClass>]
type ListTryFindNextOrHead() =
    [<TestMethod>]
    member this.EmptyList_ReturnNone() =
        do [] |> List.tryFindNextOrHead id |> Assert.equal None

    static member Integers_Data = dataArray [
        [], 5, None
        [5], 5, Some 5
        [5; 7], 5, Some 7
        [5; 7], 7, Some 5
        [3; 5; 7], 7, Some 3
        [3; 5; 7], 3, Some 5
    ]
    [<DataTestMethod>]
    [<DynamicData("Integers_Data")>]
    member this.Integers((list : int list, needle : int, result : int option)) =
        do list |> List.tryFindNextOrHead ((=) needle) |> Assert.equal result

[<TestClass>]
type SetDifferenceAll() =
    [<TestMethod>]
    member this.Empty() =
        Assert.AreEqual(Set.empty, Set.differenceAll Set.empty Set.empty)

    [<TestMethod>]
    member this.EmptyAndSomeReturnsSome() =
        let some = Set([ 1; 2; 5; 7 ])
        Assert.AreEqual(some, Set.differenceAll some Set.empty)

    [<TestMethod>]
    member this.SomeAndEmptyReturnsSome() =
        let some = Set([ 1; 2; 5; 7 ])
        Assert.AreEqual(some, Set.differenceAll Set.empty some)

    [<TestMethod>]
    member this.DisjointedReturnUnion() =
        let some = Set([ 1; 2 ])
        let other = Set([ 5; 7 ])
        Assert.AreEqual(Set([1; 2; 5; 7]), Set.differenceAll some other)

    [<TestMethod>]
    member this.SameReturnEmpty() =
        let some = Set([ 1; 2; 5; 7 ])
        let other = Set(some)
        Assert.AreEqual(Set<int>([]), Set.differenceAll some other)

    [<TestMethod>]
    member this.OverlappingReturnDifference() =
        let some = Set([ 1; 2; 5; 7 ])
        let other = Set([ 5; 7; 9; 13 ])
        Assert.AreEqual(Set<int>([1; 2; 9; 13]), Set.differenceAll some other)

[<TestClass>]
type Approximate() =
    [<TestMethod>]
    member this.equality() =
        Assert.IsTrue(5.0009 =~ 5.0);
        Assert.IsFalse(5.0011 =~ 5.0);
        Assert.IsTrue(5.0009<m> =~ 5.0<m>);
    [<TestMethod>]
    member this.notEqual() =
        Assert.IsFalse(5.0009 <>~ 5.0);
        Assert.IsTrue(5.0011 <>~ 5.0);
        Assert.IsTrue(5.0011<m> <>~ 5.0<m>);
    [<TestMethod>]
    member this.greaterThan() =
        Assert.IsTrue(5.0 >~ 5.0);
        Assert.IsTrue(4.9995 >~ 5.0);
        Assert.IsFalse(4.998 >~ 5.0);
        Assert.IsTrue(5.0<m> >~ 5.0<m>);
    [<TestMethod>]
    member this.greaterOrEqualsThan() =
        Assert.IsTrue(5.0 >=~ 5.0);
        Assert.IsTrue(4.9995 >=~ 5.0);
        Assert.IsFalse(4.998 >=~ 5.0);
        Assert.IsTrue(5.0<m> >=~ 5.0<m>);
    [<TestMethod>]
    member this.lessThan() =
        Assert.IsTrue(5.0 <~ 5.0);
        Assert.IsTrue(5.0005 <~ 5.0);
        Assert.IsFalse(5.002 <~ 5.0);
        Assert.IsTrue(5.0<m> <~ 5.0<m>);
    [<TestMethod>]
    member this.lessOrEqualThan() =
        Assert.IsTrue(5.0 <=~ 5.0);
        Assert.IsTrue(5.0005 <=~ 5.0);
        Assert.IsFalse(5.002 <=~ 5.0);
        Assert.IsTrue(5.0<m> <=~ 5.0<m>);

[<TestClass>]
type Subsequence() =
    let equal a b = a = b
    static member LeftFillings_Data = dataArray [
        "aa",   "aa",   [-1; 0]
        "aa",   "aaa",   [-1; 0]
        "aaa",   "aa",   [-2; -1]
        "aabb", "bbbb", [-2; -3]
        "bbaa", "bbbb", []
    ]
    [<DataTestMethod>]
    [<DynamicData("LeftFillings_Data")>]
    member this.LeftFillings((needle : string, haystack : string, result : int list)) =
        Seq.leftFillings needle haystack equal |> Set
        |> Assert.equal (Set(result))

    static member RightFillings_Data = dataArray [
        "aa",   "aa",   [0; 1]
        "aa",   "aaa",  [1; 2]
        "aaa",   "aa",  [0; 1]
        "bbaa", "bbbb", [2; 3]
        "aabb", "bbbb", []
    ]
    [<DataTestMethod>]
    [<DynamicData("RightFillings_Data")>]
    member this.RightFillings((needle : string, haystack : string, result : int list)) =
        Seq.rightFillings needle haystack equal |> Set
        |> Assert.equal (Set(result))

    static member MiddleFillings_Data = dataArray [
        "aa",  "aa",   [0]
        "aaa", "aa",   []
        "aa",  "aaa",  [0; 1]
        "aa",  "aa",   [0]
        "ab",  "aba",  [0]
        "bb",  "abba", [1]
    ]
    [<DataTestMethod>]
    [<DynamicData("MiddleFillings_Data")>]
    member this.MiddleFillings((needle : string, haystack : string, result : int list)) =
        Seq.middleFillings needle haystack equal |> Set
        |> Assert.equal (Set(result))

    static member AllFillings_Data = dataArray [
            "aa",  "aa",   [-1; 0; 1]
            "aaa", "aa",   [-2; -1; 0; 1]
            "aa",  "aaa",  [-1; 0; 1; 2]
            "aa",  "aa",   [-1; 0; 1]
            "ab",  "aba",  [0; 2]
            "bb",  "abba", [1]
            "abba",  "bb", [-1]
        ]

    [<DataTestMethod>]
    [<DynamicData("AllFillings_Data")>]
    member this.AllFillings((needle : string, haystack : string, expected : int list)) =
        let actual = Seq.allFillings needle haystack equal
        actual
        |> Set
        |> Assert.equal (Set expected)
