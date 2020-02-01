module Effect

let replaceAt index value list =
    let before = list |> List.take index
    let after = list |> List.skip (index + 1) |> List.take (Seq.length list - index - 1)
    before @ [ value ] @ after


let apply (world : World) (effect : Effect) =
    match effect with
    | Damage (id, amount) -> 
        match id with
        | Id index -> 
            if index >= 0 then
                let affected = world.Enemies.[index]
                let affected = { affected with Health = affected.Health - amount }
                { world with Enemies = world.Enemies |> replaceAt index affected }
            else
                { world with
                    Dude = world.Dude |> Option.map(fun dude -> { dude with Health = dude.Health - amount }) }
        
