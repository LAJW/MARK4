module Geometry
open System
open QuantumConcepts.Formats.StereoLithography

type Triangle<[<Measure>]'u> = Vec<'u> * Vec<'u> * Vec<'u>
type Mesh<[<Measure>]'u> = Triangle<'u> list

type Line<[<Measure>]'u> =
| NonVertical of float * float<'u>
| Vertical of float<'u>
with
    static member equal (first : Line<'u>) (second : Line<'u>) = 
        match first with
        | NonVertical(a1, b1) ->
            match second with
            | NonVertical(a2, b2) ->
                a1 =~ a2 && b1 =~ b2
            | Vertical _ -> false
        | Vertical x1 ->
            match second with
            | Vertical x2 ->
                x1 =~ x2
            | NonVertical _ -> false

type Segment<[<Measure>]'u> = Vec<'u> pair
module Segment =
    /// Get a vector from the first to the second point in a segment
    let diff ((a, b) : Segment<'u>) : Vec<'u> = b - a

    /// Get directional vector of a segment pointing in the direction of the
    /// second point
    let dir (segment : Segment<'u>) : Vec<1> =
        segment |> diff |> Vec.normalize

    /// Length of the segment
    let length this = this |> diff |> Vec.length 

    /// Move segment by a given vector
    let add (vec : Vec<'u>) ((a, b) : Segment<'u>) =
        a + vec, b + vec

    /// Convert segment to y = ax + b - style line
    let toLine (first : Vec<'u>, second : Vec<'u>) : Line<'u> =
        let x1, y1 = Vec.toPair first
        let x2, y2 = Vec.toPair second
        let dx = x1 - x2
        if dx =~ 0.<_> then
            Vertical(x1)
        else
            let a = (y1 - y2) / dx
            let b = y1 - a * x1
            NonVertical(a, b)

    let bounds ((a, b) : Segment<'u>) : Segment<'u> =
        let minX = min a.x b.x
        let minY = min a.y b.y
        let maxX = max a.x b.x
        let maxY = max a.y b.y
        vec(minX, minY), vec(maxX, maxY)

    let contains (other : Segment<'u>) (this : Segment<'u>) =
        if Line.equal (toLine this) (toLine other) then
            let bounds = bounds this
            (fst other |> Vec.inRect bounds) && (snd other |> Vec.inRect bounds)
        else
            false

    let destruct (segment : Segment<'u>) =
        let v1, v2 = segment
        v1.x, v1.y, v2.x, v2.y

    let intersect (s1 : Segment<'u>) (s2 : Segment<'u>) =
        // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
        // Method using determinants
        let x1, y1, x2, y2 = destruct s1
        let x3, y3, x4, y4 = destruct s2
        let b = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        if b = 0.<_> then
            None
        else
            let tl = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
            let tr = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
            let lineIntersection = vec(tl / b, tr / b)
            Some(lineIntersection)
            |> Option.filter (Vec.inRect (bounds s1))
            |> Option.filter (Vec.inRect (bounds s2))


    let samplify (resolution : float<'u>) (ray : Segment<'u>) =
        let origin = fst ray
        let dir = ray |> dir
        seq {
            let mutable prev = Option<int * int>.None
            let mutable i = 0.0<_>
            while i <= length ray do
                let pos = (origin + dir * i) |> Vec.map(fun x -> floor(x / resolution))
                let result = (int pos.x), (int pos.y)
                match prev with
                | None -> yield result
                | Some prev ->
                    if result <> prev then
                        let x1, y1 = prev
                        let x2, y2 = result
                        if x1 <> x2 && y1 <> y2 then
                            yield x1, y2
                            yield x2, y1
                        yield result
                prev <- Some result
                i <- i + resolution;
        }

    /// Orient the segment so that the first vertex is closer to 0, 0 than the
    /// second.
    /// Use when comparing two segments.
    let orient ((a, b) : Segment<'u>) : Segment<'u> =
        if (a |> Vec.length) <= (b |> Vec.length) then
            a, b
        else b, a
