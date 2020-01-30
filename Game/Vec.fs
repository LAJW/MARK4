[<AutoOpen>]
module Vec
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System

// Disable warning for downcasting to a type with a unit of measure: Vec<'u>
#nowarn "1240"

type Vec<[<Measure>] 'u>(x : float<'u>, y : float<'u>) =
    static member Zero = Vec<'u>(0.<_>, 0.<_>)
    member this.x = x
    member this.y = y

    override this.Equals other =
        match other with
        | :? Vec<'u> as other -> other.x =~ this.x && other.y =~ this.y
        | _ -> false

    override this.ToString() = "[" + x.ToString() + "; " + y.ToString() + "]"

    override this.GetHashCode() = (this.x |> round2 4, this.y |> round2 4).GetHashCode()

    interface IComparable<Vec<'u>> with
        member this.CompareTo(other: Vec<'u>) = 
            compare (this.x, this.y) (other.x, other.y)

    interface IComparable with
        member this.CompareTo(obj : System.Object) =
            match obj with
              | null -> 1
              | :? Vec<_> as other -> (this :> IComparable<_>).CompareTo other
              | _ -> invalidArg "obj" "not a Category"

let vec (x, y) = Vec(x, y)

type Vec<'u> with
    static member map (fn : float<'u> -> float<'r>) (v: Vec<'u>) = vec(fn v.x, fn v.y)
    static member (+) (a : Vec<'u>, b : Vec<'u>) = vec(a.x + b.x, a.y + b.y)
    static member (-) (a : Vec<'u>, b : Vec<'u>) = vec(a.x - b.x, a.y - b.y)
    static member (*) (v : Vec<'u>, a) = v |> Vec.map ((*) a)
    static member (*) (a, v: Vec<'u>) = v |> Vec.map ((*) a)
    static member (/) (v: Vec<'u>, a : float<_>) =
        if a = 0.<_> then None else v |> Vec.map(fun p -> p / a) |> Some
    static member length (v: Vec<'u>) = sqrt(v.x * v.x + v.y * v.y)
    static member div a (v: Vec<'u>) = v / a
    static member normalize (v: Vec<'u>) =
        let length = v |> Vec.length 
        if length = 0.<_> then vec(0., 0.) else v |> Vec.map(fun a -> a / length)
    static member rotate (angle : float<rad>) (v : Vec<'u>) =
        let a = float angle
        vec(v.x * cos a - v.y * sin a,
            v.x * sin a + v.y * cos a)
    static member confine (bounds : Vec<'u> pair) (pos : Vec<'u>) =
        let lb, ub = bounds
        vec(pos.x |> confine lb.x ub.x,
            pos.y |> confine lb.y ub.y)
    static member confineToSize (size : Vec<'u>) = Vec.confine (Vec<'u>.Zero, size)
    static member atan (vec : Vec<'u>) =
        atan2 (vec.y |> float) (vec.x |> float) * 1.<rad>
    static member inRect (square : Vec<'u> pair) (pos : Vec<'u>) =
        let a, b = square
        let xl, yl = (min a.x b.x), (min a.y b.y)
        let xu, yu = (max a.x b.x), (max a.y b.y)
        let x, y = pos.x, pos.y
        xl <= x && yl <= y && xu >= x && yu >= y

    static member inProximity (origin : Vec<'u>) (distance : float<'u>) (point : Vec<'u>) =
        Vec.length(point - origin) <= distance

    static member X (this : Vec<'u>) = this.x
    static member Y (this : Vec<'u>) = this.y

    static member toPair (this : Vec<'u>) = this.x, this.y

    static member inSegment (segment : Vec<'u> pair) (this : Vec<'u>) = 
        let a, b = segment
        let hay = b - a
        let needle = this - a
        this = a ||
        (Vec.normalize hay) = (Vec.normalize needle) && (Vec.length needle) <= Vec.length(hay)

/// Convert an angle in radians into a directional vector
let versor (angle : float<rad>) : Vec<1> =
    vec(1., 0.) |> Vec.rotate angle

let inTriangle (s : Vec<'u>) ((a, b, c) : Vec<'u> * Vec<'u> * Vec<'u>) =
    // Source: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
    let asX = s.x - a.x;
    let asY = s.y - a.y;
    let sAb = (b.x - a.x) * asY - (b.y - a.y) * asX > 0.0<_>
    if (c.x - a.x) * asY - (c.y - a.y) * asX > 0.<_> = sAb then false
    else (c.x - b.x) * (s.y - b.y) - (c.y - b.y) * (s.x - b.x) > 0.<_> = sAb
