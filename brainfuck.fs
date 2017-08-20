module BrainFuck
open System
open System.IO

// The type of IO call
type SysCall =
    | PutChar
    | GetChar
    | Stop

// The commands that are possible
type AST =
    | ModMemPtr of int// <>
    | ModMem of int // +-
    | SetMem of int // For optimised set
    | SysCall of SysCall // .,
    | Loop of AST list// []

// The OPS The VM understands
type VMOp =
    | ModMemPtr of int
    | ModMem of int
    | SetMem of int
    | SysCall of SysCall
    | JumpIfNotZero of int
    | JumpIfZero of int

// The state of the VM
type VMState = {
    program: VMOp array;
    mutable programPtr: int;
    memory: int array;
    mutable memoryPtr: int;
    mutable output: string;
    mutable input: string;
}

let parseSyntax source =
    source
        |> Seq.fold (
            fun (currentBlock, blockStack) c ->
                    match c with
                        | '+' -> AST.ModMem +1 :: currentBlock, blockStack
                        | '-' -> AST.ModMem -1 :: currentBlock, blockStack
                        | '>' -> AST.ModMemPtr +1 :: currentBlock, blockStack
                        | '<' -> AST.ModMemPtr -1 :: currentBlock, blockStack
                        | ',' -> AST.SysCall SysCall.GetChar :: currentBlock, blockStack
                        | '.' -> AST.SysCall SysCall.PutChar :: currentBlock, blockStack
                        | '[' -> [], currentBlock :: blockStack
                        | ']' ->
                            match blockStack with
                                | [] -> failwith "Closing without corrosponding opening"
                                | head :: tail ->
                                    AST.Loop (List.rev currentBlock) :: head, tail

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
                    | (AST.ModMem m, (AST.ModMem n)::rest) -> (AST.ModMem (m+n))::rest
                    | (AST.ModMemPtr m, (AST.ModMemPtr n)::rest) -> (AST.ModMemPtr (m+n))::rest
                    | (AST.Loop sl, l) -> AST.Loop (collapseSeq sl) :: l
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
                    | AST.Loop sl ->
                        match sl.Length, sl.Head with
                            | 1, AST.ModMem n -> AST.SetMem 0 :: l
                            | _ -> AST.Loop (clearLoop sl) :: l
                    | _ -> e::l

        ) []
        |> List.rev

    ast
    |> collapseSeq
    |> clearLoop

let buildProgram source =
    let ast =
        source
        |> parseSyntax
        |> optimise


    let rec translateOp =
        function
        | AST.ModMem n -> [ VMOp.ModMem n ]
        | AST.ModMemPtr n -> [ VMOp.ModMemPtr n ]
        | AST.SetMem n -> [ VMOp.SetMem n ]
        | AST.SysCall c -> [ VMOp.SysCall c ]
        | AST.Loop loopList ->
            let loop = loopList |> List.collect translateOp
            let endJump = VMOp.JumpIfZero (loop.Length + 2)
            let startJump = VMOp.JumpIfNotZero -(loop.Length)
            endJump :: loop @ [ startJump ]

    // We use an index of the jump with the offsets value of the jump to get the correct jump locs
    let offsetJump i =
        function
        | VMOp.JumpIfNotZero n -> VMOp.JumpIfNotZero (n + i)
        | VMOp.JumpIfZero n -> VMOp.JumpIfZero (n + i)
        | op -> op

    let translateAst = List.collect translateOp >> List.mapi offsetJump

    let vmOps = translateAst ast

    // Because we're going to do indexing for handling our jumps we're going to use arrays for O(1) access time of elements.
    vmOps |> List.toArray

let createVM program =
    {
        program = program;
        programPtr = 0;
        memory = Array.zeroCreate 30000;
        memoryPtr = 0;
        output = "";
        input = "";
    }

let writeInput vm input =
    vm.input <- input

let readOutput vm =
    let output = vm.output
    vm.output <- ""
    output

let runOne vm =

    let op =
        match vm.programPtr with
        | e when e >= vm.program.Length ->
            SysCall Stop
        | e -> vm.program.[vm.programPtr]

    match op with
    | VMOp.ModMemPtr n ->
        vm.memoryPtr <- (vm.memoryPtr + n + vm.memory.Length) % vm.memory.Length
        vm.programPtr <- vm.programPtr + 1
    | VMOp.ModMem n ->
        vm.memory.[vm.memoryPtr] <- (vm.memory.[vm.memoryPtr] + n + 256) % 256
        vm.programPtr <- vm.programPtr + 1
    | VMOp.SetMem n ->
        vm.memory.[vm.memoryPtr] <- (n + 256) % 256
        vm.programPtr <- vm.programPtr + 1
    | VMOp.JumpIfNotZero l ->
        match vm.memory.[vm.memoryPtr] with
            | 0 ->
                vm.programPtr <- vm.programPtr + 1
            | _ ->
                vm.programPtr <- l
    | VMOp.JumpIfZero l ->
        match vm.memory.[vm.memoryPtr] with
            | 0 ->
                vm.programPtr <- l
            | _ ->
                vm.programPtr <- vm.programPtr + 1
    | VMOp.SysCall sysCall ->
        match sysCall with
        | SysCall.PutChar ->
            let asciiCode = char vm.memory.[vm.memoryPtr]
            vm.output <- vm.output + string asciiCode
        | SysCall.GetChar ->
            let nextChar = vm.input.Chars 0
            vm.input <- vm.input.Remove(0,1)
            vm.memory.[vm.memoryPtr] <- int nextChar
        | SysCall.Stop ->
            vm.memoryPtr <- vm.memory.Length
        vm.programPtr <- vm.programPtr + 1

let runAll vmState =
    while vmState.programPtr < vmState.program.Length do
        runOne vmState