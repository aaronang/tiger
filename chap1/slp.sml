type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun maxargs (CompoundStm(left, right)) = Int.max(maxargs(left), maxargs(right))
  | maxargs (AssignStm(_, exp)) = expargs(exp)
  | maxargs (PrintStm(exps)) = Int.max(length(exps), explist(exps))

and expargs (EseqExp(stm, exp)) = Int.max(maxargs(stm), expargs(exp))
  | expargs (OpExp(left, _, right)) = Int.max(expargs(left), expargs(right))
  | expargs (_) = 0

and explist ([]) = 0
  | explist (e::t) = Int.max(expargs(e), explist(t))
