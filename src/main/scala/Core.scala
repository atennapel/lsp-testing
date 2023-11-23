object Core:
  type PosInfo = (Int, Int) // line, col
  type Span = (PosInfo, PosInfo)
  type Name = String

  private inline def posInSpan(pos: PosInfo, span: Span): Boolean =
    if pos._1 > span._1._1 && pos._1 < span._2._1 then true
    else if pos._1 == span._1._1 && pos._1 == span._2._1 then
      pos._2 >= span._1._2 && pos._2 <= span._2._2
    else if pos._1 == span._1._1 then pos._2 >= span._1._2
    else if pos._1 == span._2._1 then pos._2 <= span._2._2
    else false
  private inline def posInSpanTy(pos: PosInfo, span: Span, ty: Ty): Option[Ty] =
    if posInSpan(pos, span) then Some(ty) else None

  enum Ty:
    case TNat
    case TBool
    case TFun(pty: Ty, rty: Ty)

    override def toString: String = this match
      case TNat           => "Nat"
      case TBool          => "Bool"
      case TFun(pty, rty) => s"($pty -> $rty)"

    def pretty: String =
      def flatten(t: Ty): List[Ty] = t match
        case TFun(pty, rty) => pty :: flatten(rty)
        case t              => List(t)
      this match
        case TNat  => "Nat"
        case TBool => "Bool"
        case t @ TFun(_, _) =>
          flatten(t)
            .map {
              case t @ TFun(_, _) => s"(${t.pretty})"
              case t              => t.pretty
            }
            .mkString(" -> ")
  export Ty.*

  final case class Def(name: Name, span: Span, ty: Ty, value: Tm):
    override def toString: String = s"def $name : $ty = $value"

    def typeAt(pos: PosInfo): Option[Ty] =
      posInSpanTy(pos, span, ty).orElse(value.typeAt(pos))

  enum Tm:
    case Var(name: Name, ty: Ty, span: Span)
    case Global(name: Name, ty: Ty, span: Span)
    case NatLit(value: Int, span: Span)
    case BoolLit(value: Boolean, span: Span)
    case Succ(span: Span)
    case Lam(name: Name, span: Span, ty: Ty, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Let(name: Name, span: Span, ty: Ty, value: Tm, body: Tm)
    case If(span: Span, rty: Ty, cond: Tm, ifTrue: Tm, ifFalse: Tm)
    case Iterate(span: Span, rty: Ty, scrut: Tm, ifZ: Tm, ifS: Tm)

    override def toString: String = this match
      case Var(x, _, _)           => s"$x"
      case Global(x, _, _)        => s"$x"
      case NatLit(n, _)           => s"$n"
      case Succ(_)                => "S"
      case BoolLit(b, _)          => if b then "True" else "False"
      case Lam(x, _, t, b)        => s"(\\($x : $t) => $b)"
      case App(f, a)              => s"($f $a)"
      case Let(x, _, t, v, b)     => s"(let $x : $t = $v; $b)"
      case If(_, _, c, a, b)      => s"(if $c then $a else $b)"
      case Iterate(_, _, n, z, s) => s"(iterate $n $z $s)"

    def typeAt(pos: PosInfo): Option[Ty] = this match
      case Var(_, ty, span)    => posInSpanTy(pos, span, ty)
      case Global(_, ty, span) => posInSpanTy(pos, span, ty)
      case NatLit(_, span)     => posInSpanTy(pos, span, TNat)
      case BoolLit(_, span)    => posInSpanTy(pos, span, TBool)
      case Succ(span)          => posInSpanTy(pos, span, TFun(TNat, TNat))
      case Lam(x, span, ty, b) =>
        posInSpanTy(pos, span, ty).orElse(b.typeAt(pos))
      case App(fn, arg) => fn.typeAt(pos).orElse(arg.typeAt(pos))
      case Let(x, span, ty, v, b) =>
        posInSpanTy(pos, span, ty).orElse(v.typeAt(pos)).orElse(b.typeAt(pos))
      case If(span, ty, c, a, b) =>
        posInSpanTy(pos, span, ty)
          .orElse(c.typeAt(pos))
          .orElse(a.typeAt(pos))
          .orElse(b.typeAt(pos))
      case Iterate(span, ty, n, z, s) =>
        posInSpanTy(pos, span, ty)
          .orElse(n.typeAt(pos))
          .orElse(z.typeAt(pos))
          .orElse(s.typeAt(pos))
  export Tm.*
