object Surface:
  type PosInfo = (Int, Int) // line, col
  type Name = String

  enum Ty:
    case TNat
    case TFun(pty: Ty, rty: Ty)
    case THole
    case TPos(pos: PosInfo, ty: Ty)

    override def toString: String = this match
      case TNat           => "Nat"
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
    case Hole(name: Option[Name])
    case Lam(name: Name, ty: Option[Ty], body: Tm)
    case App(fn: Tm, arg: Tm)
    case Let(name: Name, ty: Option[Ty], value: Tm, body: Tm)
    case Pos(pos: PosInfo, tm: Tm)

    override def toString: String = this match
      case Var(x)                => s"$x"
      case NatLit(n)             => s"$n"
      case Hole(None)            => "_"
      case Hole(Some(x))         => "_$x"
      case Lam(x, Some(t), b)    => s"(\\($x : $t) => $b)"
      case Lam(x, None, b)       => s"(\\$x => $b)"
      case App(f, a)             => s"($f $a)"
      case Let(x, Some(t), v, b) => s"(let $x : $t = $v; $b)"
      case Let(x, t, v, b)       => s"(let $x = $v; $b)"
      case Pos(p, t)             => t.toString
  export Tm.*
