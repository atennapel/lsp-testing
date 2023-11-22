object Surface:
  type PosInfo = (Int, Int) // line, col
  type Name = String

  enum Ty:
    case TNat
    case TBool
    case TFun(pty: Ty, rty: Ty)
    case THole
    case TPos(pos: PosInfo, ty: Ty)

    override def toString: String = this match
      case TNat           => "Nat"
      case TBool          => "Bool"
      case TFun(pty, rty) => s"($pty -> $rty)"
      case THole          => "_"
      case TPos(_, t)     => t.toString
  export Ty.*

  final case class Def(pos: PosInfo, name: Name, ty: Option[Ty], value: Tm):
    override def toString: String = ty match
      case Some(t) => s"def $name : $t = $value"
      case None    => s"def $name = $value"

  enum Tm:
    case Var(name: Name)
    case NatLit(value: Int)
    case Succ
    case BoolLit(value: Boolean)
    case Hole(name: Option[Name])
    case Lam(name: Name, ty: Option[Ty], body: Tm)
    case App(fn: Tm, arg: Tm)
    case Let(name: Name, ty: Option[Ty], value: Tm, body: Tm)
    case If(cond: Tm, ifTrue: Tm, ifFalse: Tm)
    case Iterate(scrut: Tm, ifZ: Tm, ifS: Tm)
    case Pos(pos: PosInfo, tm: Tm)

    override def toString: String = this match
      case Succ                  => "S"
      case Var(x)                => s"$x"
      case NatLit(n)             => s"$n"
      case BoolLit(b)            => if b then "True" else "False"
      case Hole(None)            => "_"
      case Hole(Some(x))         => s"_$x"
      case Lam(x, Some(t), b)    => s"(\\($x : $t) => $b)"
      case Lam(x, None, b)       => s"(\\$x => $b)"
      case App(f, a)             => s"($f $a)"
      case Let(x, Some(t), v, b) => s"(let $x : $t = $v; $b)"
      case Let(x, t, v, b)       => s"(let $x = $v; $b)"
      case If(c, a, b)           => s"(if $c then $a else $b)"
      case Iterate(n, z, s)      => s"(iterate $n $z $s)"
      case Pos(p, t)             => t.toString
  export Tm.*
