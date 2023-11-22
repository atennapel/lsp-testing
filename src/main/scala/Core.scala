object Core:
  type Name = String

  enum Ty:
    case TNat
    case TFun(pty: Ty, rty: Ty)

    override def toString: String = this match
      case TNat           => "Nat"
      case TFun(pty, rty) => s"($pty -> $rty)"
  export Ty.*

  final case class Def(name: Name, ty: Ty, value: Tm):
    override def toString: String = s"def $name : $ty = $value"

  enum Tm:
    case Var(name: Name)
    case Global(name: Name)
    case NatLit(value: Int)
    case Lam(name: Name, ty: Ty, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Let(name: Name, ty: Ty, value: Tm, body: Tm)

    override def toString: String = this match
      case Var(x)          => s"$x"
      case Global(x)       => s"$x"
      case NatLit(n)       => s"$n"
      case Lam(x, t, b)    => s"(\\($x : $t) => $b)"
      case App(f, a)       => s"($f $a)"
      case Let(x, t, v, b) => s"(let $x : $t = $v; $b)"
  export Tm.*
