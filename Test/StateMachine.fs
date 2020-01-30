namespace Test.StateMachine

open Microsoft.VisualStudio.TestTools.UnitTesting
open StateMachine
open Test.Utils

[<TestClass>]
type ForLoop() =
    [<TestMethod>]
    member this.EmptyLoop() =
        let machine = StateMachineManager(sm {
            for item in Seq.empty do
                yield item
        })
        let result = machine.Update()
        result |> Assert.equal []

