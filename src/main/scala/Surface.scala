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

  enum Def:
    case DDef(
        pos: PosInfo,
        name: Name,
        namepos: PosInfo,
        ty: Option[Ty],
        value: Tm
    )
    case DImport(pos: PosInfo, uri: String)

    override def toString: String = this match
      case DDef(_, name, _, ty, value) =>
        ty match
          case Some(t) => s"def $name : $t = $value"
          case None    => s"def $name = $value"
      case DImport(_, uri) => s"import \"$uri\""

    def imports: Option[DImport] = this match
      case x @ DImport(pos, uri) => Some(x)
      case _                     => None
  export Def.*

  enum Tm:
    case Var(name: Name, pos: PosInfo)
    case NatLit(value: Int)
    case Succ
    case BoolLit(value: Boolean)
    case Hole(name: Option[Name])
    case Lam(name: Name, namepos: PosInfo, ty: Option[Ty], body: Tm)
    case App(fn: Tm, arg: Tm)
    case Let(name: Name, namepos: PosInfo, ty: Option[Ty], value: Tm, body: Tm)
    case If(cond: Tm, ifTrue: Tm, ifFalse: Tm)
    case Iterate(scrut: Tm, ifZ: Tm, ifS: Tm)
    case Pos(pos: PosInfo, tm: Tm)

    override def toString: String = this match
      case Succ                     => "S"
      case Var(x, _)                => s"$x"
      case NatLit(n)                => s"$n"
      case BoolLit(b)               => if b then "True" else "False"
      case Hole(None)               => "_"
      case Hole(Some(x))            => s"_$x"
      case Lam(x, _, Some(t), b)    => s"(\\($x : $t) => $b)"
      case Lam(x, _, None, b)       => s"(\\$x => $b)"
      case App(f, a)                => s"($f $a)"
      case Let(x, _, Some(t), v, b) => s"(let $x : $t = $v; $b)"
      case Let(x, _, t, v, b)       => s"(let $x = $v; $b)"
      case If(c, a, b)              => s"(if $c then $a else $b)"
      case Iterate(n, z, s)         => s"(iterate $n $z $s)"
      case Pos(p, t)                => t.toString
  export Tm.*
