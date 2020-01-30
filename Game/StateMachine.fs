module StateMachine

open FSharp.Data.UnitSystems.SI.UnitSymbols

[<RequireQualifiedAccess>]
module Detail =
    /// T - value type
    /// U - condition type
    /// V - list element type

    type T<'T, 'U, 'V> =
    | Checked of ('U -> bool) * (unit -> T<'T, 'U, 'V>) * 'V list
    | Lazy of (unit -> T<'T, 'U, 'V>) * 'V list
    | Value of 'T * 'V list

    let rec bind (func : 'T1 -> T<'T, 'U, 'V>) (this : T<'T1, 'U, 'V>) : T<'T, 'U, 'V> =
        match this with
        | Checked(condition, value, items) ->
            Checked(condition, (fun() -> bind func (value())), items)
        | Lazy(value, items) -> Lazy((fun() -> bind func (value())), items)
        | Value(value, items) ->
            match func value with
            | Checked(condition, value, more) -> Checked(condition, value, items @ more)
            | Lazy(value, more) -> Lazy(value, items @ more)
            | Value(value, more) -> Value(value, items @ more)

    let rec step (condArg : 'U) (this : T<'T, 'U, 'V>) : T<'T, 'U, 'V> * 'V list =
        match this with
        | Checked(condition, value, items) ->
            if condition(condArg) then
                value(), items
            else Checked(condition, value, []), items
        | Lazy(value, items) ->
            let result, more = step condArg (value())
            result, items @ more
        | Value(value, items) -> Value(value, []), items

    let delay (func : unit -> T<'T, 'U, 'V>) : T<'T, 'U, 'V> =
        Lazy((fun () -> func()), [])

    let result value = Value(value, [])

    let rec whileLoop (pred : unit -> bool) (body : T<'T, 'U, 'V>) =
        if pred() then body |> bind (fun _ -> whileLoop pred body)
        else result ()

    let combine expr1 expr2 =
        expr1 |> bind (fun () -> expr2)

    let yieldItem item = Value(ignore(), [ item ])

    let forLoop (collection:seq<_>) func =
        let ie = collection.GetEnumerator()
        whileLoop 
            (fun () -> ie.MoveNext()) 
            (delay (fun () -> let value = ie.Current in func value))

    type Builder() =
        member this.Bind(x, f) = bind f x
        member this.While(cond, x) = whileLoop cond x
        member this.For(collection, func) = forLoop collection func
        member this.Delay(func) = delay func
        member this.Zero() = result ()
        member this.Yield(item) = yieldItem(item)
        member this.Combine(expr1, expr2) = combine expr1 expr2
        member this.Return value = result value
        member this.ReturnFrom value = value


type StateMachine<'T, 'U, 'V> = Detail.T<'T, 'U, 'V>

let sm = Detail.Builder()

let waitOnce() =
    StateMachine.Checked(
        (fun _ -> true),
        (fun () -> Detail.result ()), [])

let waitFor(expected : float<s>) =
    let mutable remaining = expected
    StateMachine.Checked(
        (fun (dt : float<s>) ->
            if remaining <= dt then
                true
            else
                do remaining <- remaining - dt
                false),
        (fun () -> Detail.result ()), [])

let waitUntil(callback) =
    StateMachine.Checked(
        (fun _ -> callback()),
        (fun () -> Detail.result ()), [])

type StateMachineManager<'T, 'U, 'V>(machine : StateMachine<'T, 'U, 'V>) =
    let mutable state = machine
    member this.Update (condArg : 'U) : 'V list =
        let nextState, result = state |> Detail.step(condArg)
        state <- nextState
        result
        


