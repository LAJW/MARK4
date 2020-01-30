namespace Test.Geometry.Segment

open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Geometry

[<TestClass>]
type Destruct() =
    [<TestMethod>]
    member this.main() =
        let segment = vec(1.<m>, 2.<m>), vec(3.<m>, 4.<m>)
        Assert.AreEqual((1.<m>, 2.<m>, 3.<m>, 4.<m>), segment |> Segment.destruct)

[<TestClass>]
type Length() =
    [<TestMethod>]
    member this.nonZero() =
        let segment = vec(1.<m>, 2.<m>), vec(4.<m>, 6.<m>)
        Assert.AreEqual(segment |> Segment.length |> float, 5., delta)
    [<TestMethod>]
    member this.zero() =
        let segment = vec(1.<m>, 2.<m>), vec(1.<m>, 2.<m>)
        Assert.AreEqual(segment |> Segment.length |> float, 0., delta)

[<TestClass>]
type Dir() =
    [<TestMethod>]
    member this.zero() =
        let result = (vec(1.<m>, 2.<m>), vec(1.<m>, 2.<m>)) |> Segment.dir |> Vec.map float
        Assert.AreEqual(result.x, 0., delta)
        Assert.AreEqual(result.y, 0., delta)
    [<TestMethod>]
    member this.nonZero() =
        let result = (vec(1.<m>, 2.<m>), vec(2.<m>, 3.<m>)) |> Segment.dir |> Vec.map float
        Assert.AreEqual(result.x, 1. / sqrt(2.), delta)
        Assert.AreEqual(result.y, 1. / sqrt(2.), delta)

[<TestClass>]
type Intersect () =
    let intersect = Segment.intersect

    [<TestMethod>]
    member this.TestMethodPassing() =
        Assert.IsTrue(true)

    [<TestMethod>]
    member this.IntersectionBasic() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(1.0<m>, 1.0<m>))
                (vec(0.0<m>, 1.0<m>), vec(1.0<m>, 0.0<m>))
        Assert.IsTrue(intersection.IsSome)
        let vec = intersection |> Option.get |> Vec.map float
        Assert.AreEqual(vec.x, 0.5, delta)
        Assert.AreEqual(vec.y, 0.5, delta)

    [<TestMethod>]
    member this.IntersectionHorizontal() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(2.0<m>, 1.0<m>))
                (vec(0.0<m>, 1.0<m>), vec(2.0<m>, 0.0<m>))
        Assert.IsTrue(intersection.IsSome)
        let vec = intersection |> Option.get |> Vec.map float
        Assert.AreEqual(vec.x, 1.0, delta)
        Assert.AreEqual(vec.y, 0.5, delta)

    [<TestMethod>]
    member this.IntersectionCheckBoundariesIncludeVertex() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(1.0<m>, 1.0<m>))
                (vec(0.0<m>, 2.0<m>), vec(1.0<m>, 1.0<m>))
        Assert.IsTrue(intersection.IsSome)
        let vec = intersection |> Option.get |> Vec.map float
        Assert.AreEqual(vec.x, 1.0, delta)
        Assert.AreEqual(vec.y, 1.0, delta)

    [<TestMethod>]
    member this.IntersectionParallel() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(1.0<m>, 0.0<m>))
                (vec(0.0<m>, 1.0<m>), vec(1.0<m>, 1.0<m>))
        Assert.IsTrue(intersection.IsNone)

    [<TestMethod>]
    member this.IntersectionParallelOverlapping() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(1.0<m>, 0.0<m>))
                (vec(0.0<m>, 0.0<m>), vec(1.0<m>, 0.0<m>))
        Assert.IsTrue(intersection.IsNone)

    [<TestMethod>]
    member this.IntersectionParallelOverlappingHorizontal() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(0.0<m>, 1.0<m>))
                (vec(0.0<m>, 0.0<m>), vec(0.0<m>, 1.0<m>))
        Assert.IsTrue(intersection.IsNone)

    [<TestMethod>]
    member this.IntersectionCheckBoundaries() =
        let intersection =
            intersect
                (vec(0.0<m>, 0.0<m>), vec(1.0<m>, 1.0<m>))
                (vec(0.0<m>, 2.0<m>), vec(1.0<m>, 2.0<m>))
        Assert.IsTrue(intersection.IsNone)

[<TestClass>]
type Samplify () =
    let samplify = Segment.samplify
        
    [<TestMethod>]
    member this.samplifyIdCenter() =
        let blocks =
            samplify 10.<m> (vec(5.<m>, 5.<m>), vec(5.<m>, 5.<m>))
            |> Seq.toList
        Assert.AreEqual([0, 0], blocks)

    [<TestMethod>]
    member this.samplify45DiagonalOffCenter() =
        let blocks =
            samplify 10.<m> (vec(5.<m>, 4.<m>), vec(15.<m>, 14.<m>))
            |> Seq.toList
        Assert.AreEqual([
            0, 0;       // original square
            0, 1; 1, 0; // diagonal, because moved diagonally
            1, 1;       // Destination
        ], blocks)

    [<TestMethod>]
    member this.samplifyHorizontalCenter() =
        let blocks =
            samplify 10.<m> (vec(5.<m>, 5.<m>), vec(25.<m>, 5.<m>))
            |> Seq.toList
        Assert.AreEqual([
            0, 0; // original square
            1, 0; // original square
            2, 0; // Destination
        ], blocks)

    [<TestMethod>]
    member this.samplifyHorizontalEdge() =
        let blocks =
            samplify 10.<m> (vec(0.<m>, 5.<m>), vec(25.<m>, 5.<m>))
            |> Seq.toList
        Assert.AreEqual([
            0, 0; // original square
            1, 0; // original square
            2, 0; // Destination
        ], blocks)
