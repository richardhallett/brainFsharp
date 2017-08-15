module BrainFuck
open System
open System.IO

// The type of IO call
type IOCall =
    | PutChar
    | GetChar

// The commands that are possible
type AST =
    | ModMemPtr of int// <>
    | ModMem of int // +-
    | SetMem of int // For optimised set
    | IOCall of IOCall // .,
    | Loop of AST list// []

type Program = Program of AST list

type VM = {
    memory: int array;
    mutable memoryPtr: int;
}

let parseSyntax source =
    source
        |> Seq.fold (
            fun (currentBlock, blockStack) c ->
                    match c with
                        | '+' -> ModMem +1 :: currentBlock, blockStack
                        | '-' -> ModMem -1 :: currentBlock, blockStack
                        | '>' -> ModMemPtr +1 :: currentBlock, blockStack
                        | '<' -> ModMemPtr -1 :: currentBlock, blockStack
                        | ',' -> IOCall IOCall.GetChar :: currentBlock, blockStack
                        | '.' -> IOCall IOCall.PutChar :: currentBlock, blockStack
                        | '[' -> [], currentBlock :: blockStack
                        | ']' ->
                            match blockStack with
                                | [] -> failwith "Closing without corrosponding opening"
                                | head :: tail ->
                                    Loop (List.rev currentBlock) :: head, tail

                        | _ -> currentBlock, blockStack
        ) ([], [])
        |> fun (ast, _) -> ast // Reverse the top branch to match instruction order
        |> List.rev

let optimise ast =

    // Collapase sequential instructions into combined operations e.g. ++++ modifies memory by 4
    let rec collapseSeq ast =
        ast
        |> List.fold (
            fun l e ->
                match e, l with
                    | (ModMem m, (ModMem n)::rest) -> (ModMem (m+n))::rest
                    | (ModMemPtr m, (ModMemPtr n)::rest) -> (ModMemPtr (m+n))::rest
                    | (Loop sl, l) -> Loop (collapseSeq sl) :: l
                    | _ -> e::l
        ) []
        |> List.rev

    // We know if we are only doing one memory change by in a loop by any amount in then we are just setting the data to 0, e.g. [-] or [+] or [---]
    // Note technically even numbers for the memory change would cause a infinite loop but we optimise that out
    let rec clearLoop ast =
        ast
        |> List.fold (
            fun l e ->
                match e with
                    | Loop sl ->
                        match sl.Length, sl.Head with
                            | 1, ModMem n -> SetMem 0 :: l
                            | _ -> Loop (clearLoop sl) :: l
                    | _ -> e::l

        ) []
        |> List.rev

    ast
    |> collapseSeq
    |> clearLoop

let buildProgram source =
    source
    |> parseSyntax
    |> optimise
    |> Program

let exec program memoryWatch getChar putChar =
    let vm = {
        memory = Array.zeroCreate 30000;
        memoryPtr = 0;
    }

    let rec run commands vm =
        match commands with
            | command :: commands ->
                match command with
                    | ModMemPtr v ->
                        vm.memoryPtr <- (vm.memoryPtr + v + vm.memory.Length) % vm.memory.Length
                        memoryWatch vm.memory
                        run commands vm
                    | ModMem v ->
                        vm.memory.[vm.memoryPtr] <- (vm.memory.[vm.memoryPtr] + v + 256) % 256
                        memoryWatch vm.memory
                        run commands vm
                    | SetMem v ->
                        vm.memory.[vm.memoryPtr] <- (v + 256) % 256
                        memoryWatch vm.memory
                        run commands vm
                    | IOCall ioCall ->
                        match ioCall with
                            | IOCall.PutChar ->
                                let asciiCode = char vm.memory.[vm.memoryPtr]
                                putChar asciiCode
                            | IOCall.GetChar ->
                                vm.memory.[vm.memoryPtr] <- getChar ()
                        run commands vm
                    | Loop loopCommands ->
                        match vm.memory.[vm.memoryPtr] with
                            | 0 ->
                                run commands vm
                            | _ ->
                                run (loopCommands @ command :: commands) vm
            | [] -> ()

    let (Program commands) = program;
    run commands vm

let execNoWatch program getChar putChar =
    let noWatch a =
        ()
    exec program noWatch getChar putChar