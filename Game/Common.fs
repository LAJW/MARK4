[<AutoOpen>]
module Common

open LanguagePrimitives

[<Measure>]
type rad

[<Measure>]
type Mineral

type MutableList<'u> = System.Collections.Generic.List<'u>
type MutableSet<'u> = System.Collections.Generic.HashSet<'u>

type Dictionary<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

type System.Collections.Generic.Dictionary<'k, 'v> with
    member this.AddOrAssign(key : 'k, value : 'v) =
        do this.[key] <- value

    /// Add except when there's already an element don't throw
    member this.AddNoThrow(key, value) : unit =
        if not (this.ContainsKey key) then
            do this.Add(key, value)

    /// Like square brackets operator, except when the element isn't there,
    /// default to the supplied value
    member this.GetOrDefaultWith(key, value) : 'v =
        if this.ContainsKey key then
            this.[key]
        else
            do this.Add(key, value)
            value

module Dictionary =
    let tryFind (key : 'k) (dict : Dictionary<'k, 'v>) =
        match dict.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    let toSeq (dict : Dictionary<'k, 'v>) : seq<'k * 'v> =
        dict |> Seq.map(fun pair -> pair.Key, pair.Value)

type Pair<'u> = 'u * 'u
type pair<'u> = Pair<'u>

module Pair =
    let map (f : 'T -> 'U) (a : 'T, b : 'T) : 'U pair = f a, f b
    let toList (a : 'T, b : 'T) = [a; b]
    let flip (a, b) = (b, a)

let Pi = (Microsoft.Xna.Framework.MathHelper.Pi |> float) * 1.<rad>

/// <summary>
/// Round with precision function
/// </summary>
/// <example>
/// Pi |> round2 3 = 3.141
/// </example>
let round2 (precision : int) (value : float<'u>) =
    System.Math.Round(float value, precision) * FloatWithMeasure<'u>(1.)

/// Confine the value between the upper and lower bound
let confine lower upper =
    if upper < lower then invalidArg "upper" "Upper bound should be greater or equal to smaller" |> ignore
    min upper >> max lower

/// <summary>
/// Raise to the power of two (helps with units of measure)
/// </summary>
/// <remarks>
/// Can't use pown, because it doesn't work with units of measure, because n is
/// not known at compille time
/// </remarks>
let inline pow2 a = a * a

/// Convert degrees to radians
let toRad (deg : float<_>) = deg / 180. * Pi

type Optionally() =
    member this.Bind(x, f) =
        Option.bind f x

    member this.Return x = Some x

    member this.ReturnFrom x = x

let optionally = Optionally()

module List = 
    let tryNotEmpty list = 
        match list with
        | [] -> None
        | list -> Some list

    let cdr list = 
        match list with
        | [] -> []
        | _ :: rest -> rest

    let tryFindNextOrHead (pred : 'u -> bool) (list : 'u list) : 'u option =
        if list.IsEmpty then
            None
        else
            match list |> List.tryFindIndex pred with
            | Some index -> Some list.[(index + 1) % list.Length]
            | None -> Some list.Head

module Seq = 
    let allOrNothing sequence =
        let elements = sequence |> Seq.toList
        if elements |> List.forall Option.isSome
        then elements |> List.map Option.get |> Some
        else None

    let tryNotEmpty sequence = 
        if sequence |> Seq.isEmpty
        then None
        else Some sequence

    let tryMinBy fn = tryNotEmpty >> Option.map (Seq.minBy fn)

    /// <summary>Python enumerate</summary>
    let enumerate sequence =
        sequence |> Seq.mapi(fun index value -> index, value)

    let tryFindNextIndex start pred collection =
        collection
        |> Seq.skip start
        |> Seq.tryFindIndex pred
        |> Option.map ((+) start)

    let leftFillings (needle : seq<'u>) (haystack : seq<'u>) (equal : 'u -> 'u -> bool) : int list =
        let rec matchLeftFrom offset needle haystack =
            match haystack |> tryFindNextIndex offset (equal (needle |> Seq.last)) with
            | Some index -> 
                let partSize = index + 1
                if partSize <= Seq.length needle then
                    let needlePart = needle |> Seq.rev |> Seq.take partSize
                    let haystackPart = haystack |> Seq.take partSize 
                    let areEqual = Seq.zip needlePart haystackPart
                                   |> Seq.forall(fun (a, b) -> equal a b)
                    let more = matchLeftFrom partSize needle haystack
                    if areEqual
                        then [partSize - Seq.length(needle)] @ more
                    else more
                else []
            | None -> []
        matchLeftFrom 0 needle haystack

    let rightFillings
        (needle : seq<'u>) (haystack : seq<'u>) (equal : 'u -> 'u -> bool)
        : int list =
        leftFillings (Seq.rev needle) (Seq.rev haystack) equal
        |> List.map(fun index -> (Seq.length haystack) - (Seq.length needle + index))

    let middleFillings
        (needle : seq<'u>)
        (haystack : seq<'u>)
        (equal : 'u -> 'u -> bool) : int list =
        if (Seq.length needle) > (Seq.length haystack)
        then []
        else
            let needleLength = needle |> Seq.length
            let haystackLength = haystack |> Seq.length
            let rec matchLeftFrom offset needle haystack =
                match haystack |> tryFindNextIndex offset (equal (needle |> Seq.head)) with
                | Some index -> 
                    if index <= haystackLength - needleLength then
                        let haystackPart = haystack |> Seq.skip index |> Seq.take needleLength
                        let areEqual = Seq.zip needle haystackPart
                                       |> Seq.forall(fun (a, b) -> equal a b)
                        let more = matchLeftFrom (index + 1) needle haystack
                        if areEqual
                        then [index] @ more
                        else more
                    else []
                | None -> []
            matchLeftFrom 0 needle haystack

    /// <summary>
    ///   Find all possible offsets where overlapping sequences are the same
    /// </summary>
    /// <example>
    ///   foo; oof => 1
    ///   0101; 0101 => -2; 0; 2
    ///   1; 11 => 0; 1
    /// </example>
    /// <notes>
    ///   Tags: match, allMatches, matchAll
    /// </notes>
    let allFillings (needle : seq<'u>) (haystack : seq<'u>) (equal : 'u -> 'u -> bool) =
        let invertedMiddle needle haystack =
            middleFillings haystack needle equal |> List.map((*) -1)
        leftFillings needle haystack equal
        @ rightFillings needle haystack equal
        @ invertedMiddle needle haystack
        @ middleFillings needle haystack equal
        |> Set
        |> Set.toList
        

module Set =
    // Get an absolute difference between two sets
    let differenceAll a b =
        let intersection = Set.intersect a b
        Set.union (Set.difference a intersection) (Set.difference b intersection)

// Approximate operators for float
let delta = 0.001
let (=~) (a : float<'u>) (b : float<'u>) = a - b |> abs < FloatWithMeasure delta
let (<>~) a b = (a =~ b) |> not
let (>~) a b = a > b - FloatWithMeasure delta
let (>=~) a b = a >= b - FloatWithMeasure delta
let (<~) a b = a < b + FloatWithMeasure delta
let (<=~) a b = a <= b + FloatWithMeasure delta

type IResourceManager =
    /// Load an image. Takes in a single argument - path without the .png
    /// extension as described in the Content.mgcb file
    abstract member Texture : string -> Microsoft.Xna.Framework.Graphics.Texture2D
    /// Load a STL mesh. Takes in a single argument - path without the .stl
    /// extension relative to :Mark3/Content directory
    abstract member Mesh : string -> QuantumConcepts.Formats.StereoLithography.STLDocument
