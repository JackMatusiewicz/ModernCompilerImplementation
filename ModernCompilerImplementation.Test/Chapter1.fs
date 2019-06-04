namespace ModernCompilerImplementation.Test

open NUnit.Framework
open ModernCompilerImplementation

[<TestFixture>]
[<RequireQualifiedAccess>]
module Chapter1Tests =

    let private secondStatement =
        Compound
            (Assign
                 (Iden "b",
                  SeqExp (Print [IdExp (Iden "a"); BinaryExp (IdExp (Iden "a"), Subtract, NumExp (Num 1.))],
                          BinaryExp (NumExp (Num 10.), Multiply, IdExp (Iden "a"))))
            , Print [IdExp (Iden "b")])

    let private testApp =
        Compound (Assign (Iden "a", BinaryExp (NumExp (Num 5.), Add, NumExp (Num 3.))), secondStatement)

    [<Test>]
    let ``Simple test for maxArgs`` () =
        let res = StraightLineAnalyser.maxArgs testApp
        Assert.That(res, Is.EqualTo 2)

    [<Test>]
    let ``Simple test for interpret`` () =
        StraightLineAnalyser.interpret testApp
        Assert.Pass ()