object Core:
  type PosInfo = (Int, Int) // line, col
  type Name = String

  enum Ty:
    case TNat
    case TBool
    case TFun(pty: Ty, rty: Ty)

    override def toString: String = this match
      case TNat           => "Nat"
      case TBool          => "Bool"
      case TFun(pty, rty) => s"($pty -> $rty)"
  export Ty.*

  final case class Def(pos: PosInfo, name: Name, ty: Ty, value: Tm):
    override def toString: String = s"def $name : $ty = $value"

  enum Tm:
    case Var(name: Name, ty: Ty, pos: PosInfo)
    case Global(name: Name, ty: Ty, pos: PosInfo)
    case NatLit(value: Int)
    case BoolLit(value: Boolean)
    case Succ
    case Lam(name: Name, ty: Ty, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Let(name: Name, ty: Ty, value: Tm, body: Tm)
    case If(cond: Tm, ifTrue: Tm, ifFalse: Tm)
    case Iterate(scrut: Tm, ifZ: Tm, ifS: Tm)

    override def toString: String = this match
      case Var(x, _, _)     => s"$x"
      case Global(x, _, _)  => s"$x"
      case NatLit(n)        => s"$n"
      case Succ             => "S"
      case BoolLit(b)       => if b then "True" else "False"
      case Lam(x, t, b)     => s"(\\($x : $t) => $b)"
      case App(f, a)        => s"($f $a)"
      case Let(x, t, v, b)  => s"(let $x : $t = $v; $b)"
      case If(c, a, b)      => s"(if $c then $a else $b)"
      case Iterate(n, z, s) => s"(iterate $n $z $s)"
  export Tm.*
