namespace ModernCompilerImplementation


type Iden = Iden of string
type Num = Num of float

type Operator =
    | Multiply
    | Divide
    | Add
    | Subtract

type Expression =
    | IdExp of Iden
    | NumExp of Num
    | BinaryExp of Expression * Operator * Expression
    | SeqExp of Statement * Expression

and Statement =
    | Compound of Statement * Statement
    | Assign of Iden * Expression
    | Print of Expression list

[<RequireQualifiedAccess>]
module StraightLineAnalyser =

    let rec private multiCont
        (conts : (('a -> 'k) -> 'k) list)
        (c : 'a list -> 'k)
        : 'k
        =
        match conts with
        | [] -> c []
        | h::t ->
            h (fun a -> multiCont t (fun xs -> c <| a :: xs))

    let maxArgs (statement : Statement) : int =
        let rec maxFromExpression
            (exp : Expression)
            (cont : int -> int)
            : int
            =
            match exp with
            | IdExp _ -> cont 0
            | NumExp _ -> cont 0
            | BinaryExp (l,_,r) ->
                maxFromExpression
                    l
                    (fun c ->
                        maxFromExpression
                            r
                            (fun d ->
                                max c d |> cont))
            | SeqExp (l,r) ->
                maxFromStatement
                    l
                    (fun c ->
                        maxFromExpression
                            r
                            (fun d ->
                                max c d |> cont))
        and maxFromStatement
            (statement : Statement)
            (cont : int -> int)
            : int
            =
            match statement with
            | Compound (l,r) ->
                maxFromStatement
                    l
                    (fun a ->
                        maxFromStatement
                            r
                            (fun b ->
                                max a b |> cont))
            | Assign (_,s) -> maxFromExpression s cont
            | Print exps ->
                let args = List.length exps
                exps
                |> List.map maxFromExpression
                |> fun cs -> multiCont cs (List.fold (fun a b -> max a b) args >> cont)
        maxFromStatement statement id
                

    let interpret (statement : Statement) : unit =
        let rec evaluateExp
            (store : Map<Iden, float>)
            (e : Expression)
            : float * Map<Iden, float>
            =
            match e with
            | IdExp id -> Map.find id store, store
            | NumExp (Num n) -> n, store
            | BinaryExp (l,o,r) ->
                let (v, s) = evaluateExp store l
                let (v2, s) = evaluateExp s r
                match o with
                | Add -> (+)
                | Subtract -> (-)
                | Multiply -> (*)
                | Divide -> (/)
                |> fun f -> f v v2, s
            | SeqExp (s,e) ->
                let store = evaluateStatement store s
                evaluateExp store e
        and evaluateStatement (store : Map<Iden, float>) (s : Statement) : Map<Iden, float> =
            match s with
            | Compound (l,r) ->
                evaluateStatement store l
                |> fun s -> evaluateStatement s r
            | Assign (id,e) ->
                let (v, s) = evaluateExp store e
                Map.add id v s
            | Print exps ->
                exps
                |> List.fold (fun s e -> evaluateExp s e |> (fun (a, b) -> printfn "%f" a; b)) store

        evaluateStatement Map.empty statement
        |> ignore
