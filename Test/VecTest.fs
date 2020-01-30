namespace Test.Vec

open Microsoft.VisualStudio.TestTools.UnitTesting
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic

[<TestClass>]
type VecTest() =
    [<TestMethod>]
    member this.add() =
        let result = vec(1., 2.) + vec(3., 4.)
        Assert.AreEqual(4., result.x, delta)
        Assert.AreEqual(6., result.y, delta)

    [<TestMethod>]
    member this.subtract() =
        let result = vec(3., 4.) - vec(0.5, 1.0)
        Assert.AreEqual(2.5, result.x, delta)
        Assert.AreEqual(3.0, result.y, delta)

    [<TestMethod>]
    member this.multiplyPost() =
        let result = vec(3., 4.) * 2.
        Assert.AreEqual(6., result.x, delta)
        Assert.AreEqual(8., result.y, delta)

    [<TestMethod>]
    member this.multiplyPre() =
        let result = 2. * vec(3., 4.)
        Assert.AreEqual(6., result.x, delta)
        Assert.AreEqual(8., result.y, delta)

    [<TestMethod>]
    member this.divideByNonZero() =
        let result = vec(3., 4.) / 2. |> Option.get
        Assert.AreEqual(1.5, result.x, delta)
        Assert.AreEqual(2., result.y, delta)

    [<TestMethod>]
    member this.divideByZero() =
        let result = vec(3., 4.) / 0.
        Assert.IsTrue(result.IsNone)

    [<TestMethod>]
    member this.divByNonZero() =
        let result = vec(3., 4.) |> Vec.div 2. |> Option.get
        Assert.AreEqual(1.5, result.x, delta)
        Assert.AreEqual(2., result.y, delta)

    [<TestMethod>]
    member this.divByZero() =
        let result = vec(3., 4.) |> Vec.div 0.
        Assert.IsTrue(result.IsNone)

    [<DataRow(0.,  0.,  0.)>]
    [<DataRow(3.,  0.,  3.)>]
    [<DataRow(0.,  3.,  3.)>]
    [<DataRow(-3., 0.,  3.)>]
    [<DataRow(0., -3.,  3.)>]
    [<DataRow(4.,  3.,  5.)>]
    [<DataRow(3.,  4.,  5.)>]
    [<DataRow(-4., 3.,  5.)>]
    [<DataRow(-3., -4., 5.)>]
    [<DataRow(4.<m>,  3.<m>,  5.)>]
    [<DataRow(3.<m>,  4.<m>,  5.)>]
    [<DataTestMethod>]
    member this.length(x, y, length) =
        Assert.AreEqual(length, vec(x, y) |> Vec.length |> float, delta)

    [<DataRow(0., 0., 0., 0.)>]
    [<DataRow(0.<m>, 0.<m>, 0., 0.)>]
    [<DataRow(5., 0., 1., 0.)>]
    [<DataRow(0., 5., 0., 1.)>]
    [<DataRow(-5., 0., -1., 0.)>]
    [<DataRow(0., -5., 0., -1.)>]
    [<DataTestMethod>]
    member this.normalize(x, y, expectedX, expectedY) =
        let result = vec(x, y) |> Vec.normalize
        Assert.AreEqual(expectedX, result.x, delta)
        Assert.AreEqual(expectedY, result.y, delta)

    [<TestMethod>]
    member this.normalize45Degree() =
        let result = vec(-1., 1.) |> Vec.normalize
        Assert.AreEqual(-1. / sqrt(2.), result.x, delta)
        Assert.AreEqual(1. / sqrt(2.), result.y, delta)

    [<TestMethod>]
    member this.inTriangle() =
        Assert.IsTrue(inTriangle (vec(0.4, 0.4)) (vec(0., 1.), vec(0., 0.), vec(1., 0.)))
        Assert.IsFalse(inTriangle (vec(0.6, 0.6)) (vec(0., 1.), vec(0., 0.), vec(1., 0.)))

    //        Position  Size      Expected
    [<DataRow(0.,  0.,  0.,  0.,  0.,  0.)>]
    [<DataRow(1.,  -1., 0.,  0.,  0.,  0.)>]
    [<DataRow(1.,  2.,  10., 10., 1.,  2.)>]
    [<DataRow(1.,  11., 10., 10., 1.,  10.)>]
    [<DataRow(11., 2.,  10., 10., 10., 2.)>]
    [<DataRow(-1., 2.,  10., 10., 0.,  2.)>]
    [<DataRow(1.,  -2., 10., 10., 1.,  0.)>]
    [<DataTestMethod>]
    member this.confineToSize(x, y, sizeX, sizeY, expectedX, expectedY) =
        let pos = vec(x, y)
        let size = vec(sizeX, sizeY)
        let result = pos |> Vec.confineToSize size
        Assert.AreEqual(expectedX, result.x, delta)
        Assert.AreEqual(expectedY, result.y, delta)

    //        Position     Bounds               Expected
    [<DataRow(0.,   0.,    0.,  0.,   0.,  0.,  0.,   0.)>]
    [<DataRow(1.,   -1.,   0.,  0.,   0.,  0.,  0.,   0.)>]
    [<DataRow(1.,   2.,    0.,  0.,   10., 10., 1.,   2.)>]
    [<DataRow(1.,   11.,   0.,  0.,   10., 10., 1.,   10.)>]
    [<DataRow(11.,  2.,    0.,  0.,   10., 10., 10.,  2.)>]
    [<DataRow(-1.,  2.,    0.,  0.,   10., 10., 0.,   2.)>]
    [<DataRow(1.,   -2.,   0.,  0.,   10., 10., 1.,   0.)>]
    // Negative
    [<DataRow(-1.,  -2.,  -10., -10., 0.,  0.,  -1.,  -2.)>]
    [<DataRow(-1.,  -11., -10., -10., 0.,  0.,  -1.,  -10.)>]
    [<DataRow(-11., -2.,  -10., -10., 0.,  0.,  -10., -2.)>]
    [<DataRow(1.,   -2.,  -10., -10., 0.,  10.,  0.,  -2.)>]
    [<DataRow(-1.,  2.,   -10., -10., 0.,  0.,  -1.,  0.)>]
    [<DataTestMethod>]
    member this.confine (x, y, lowerBoundX, lowerBoundY, upperBoundX,
                         upperBoundY, expectedX, expectedY) =
        let pos = vec(x, y)
        let bounds = vec(lowerBoundX, lowerBoundY), vec(upperBoundX, upperBoundY)
        let result = pos |> Vec.confine bounds
        Assert.AreEqual(expectedX, result.x, delta)
        Assert.AreEqual(expectedY, result.y, delta)

[<TestClass>]
type Rotate() =
    [<TestMethod>]
    member this.rotateZero() =
        let result = vec(0., 0.) |> Vec.rotate 0.<rad>
        Assert.AreEqual(0., result.x, delta)
        Assert.AreEqual(0., result.y, delta)

    [<DataRow(42., 96.,  1., -42, -96.)>]
    [<DataRow(42., 96., -1., -42, -96.)>]
    [<DataRow(42., 96.,  2., -96,  42.)>]
    [<DataRow(42., 96., -2.,  96, -42.)>]
    [<DataTestMethod>]
    member this.rotate(x, y, divider, expectedX, expectedY) =
        let result = vec(x, y) |> Vec.rotate (Pi / divider)
        Assert.AreEqual(expectedX, result.x, delta)
        Assert.AreEqual(expectedY, result.y, delta)

    [<TestMethod>]
    member this.rotate30() =
        let result = vec(1., 0.) |> Vec.rotate (Pi / 6.)
        Assert.AreEqual(sqrt(3.) / 2., result.x, delta)
        Assert.AreEqual(0.5, result.y, delta)

    [<TestMethod>]
    member this.rotate30Back() =
        let result = vec(1., 0.) |> Vec.rotate (-Pi / 6.)
        Assert.AreEqual(sqrt(3.) / 2., result.x, delta)
        Assert.AreEqual(-0.5, result.y, delta)

[<TestClass>]
type HashCode() =
    [<TestMethod>]
    member this.useWithHashSet() =
        let vecs =
            [ Vec(0., 1.); Vec(0., 1.); Vec(1., 0.); Vec(1., 0.) ]
            |> List.toSeq
            |> HashSet
        Assert.AreEqual(2, vecs.Count)
        Assert.IsTrue(vecs.Contains(Vec(0., 1.)))
        Assert.IsTrue(vecs.Contains(Vec(1., 0.)))
        Assert.IsFalse(vecs.Contains(Vec(1., 1.)))

    [<TestMethod>]
    member this.roundingErrors() =
        let vecs =
            [
                Vec(0., 2. |> sqrt |> pow2); Vec(0., 2.)
                Vec(3. |> sqrt |> pow2, 0.); Vec(3., 0.)
            ]
            |> List.toSeq
            |> HashSet
        Assert.AreEqual(2, vecs.Count)
        Assert.IsTrue(vecs.Contains(Vec(0., 2.)))
        Assert.IsTrue(vecs.Contains(Vec(3., 0.)))
        Assert.IsFalse(vecs.Contains(Vec(3., 2.)))
