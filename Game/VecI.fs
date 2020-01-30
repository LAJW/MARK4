[<AutoOpen>]
module VecI

type VecI = { X : int; Y : int }
with
    static member create(x : int, y : int) = { X = x; Y = y }
    static member map (fn : int -> int) (this : VecI) = VecI.create(fn this.X, fn this.Y)
    static member (+) (a : VecI, b : VecI) = VecI.create(a.X + b.X, a.Y + b.Y)
    static member (-) (a : VecI, b : VecI) = VecI.create(a.X - b.X, a.Y - b.Y)
    static member (*) (v : VecI, a) = v |> VecI.map ((*) a)
    static member (*) (a, v: VecI) = v |> VecI.map ((*) a)
    static member (/) (v: VecI, a) = v |> VecI.map (fun value -> value / a)
    static member getX (this : VecI) = this.X
    static member getY (this : VecI) = this.Y
    static member toPair (this : VecI) = this.X, this.Y
    static member toVec (this : VecI) : Vec<1> = this |> VecI.toPair |> Pair.map float |> vec
    static member neighbors (pos : VecI) : VecI list = [
            pos + VecI.create(1, 0)
            pos + VecI.create(0, 1)
            pos - VecI.create(1, 0)
            pos - VecI.create(0, 1)
        ]


let veci(x, y) = { X = x; Y = y }
