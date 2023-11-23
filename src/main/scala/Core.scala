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

    def spanOf(pos: PosInfo)(implicit
        env: Map[Name, Span]
    ): Option[List[Span]] =
      value.spanOf(pos)(env + (name -> span))

    def matchPos(pos: PosInfo): Boolean = posInSpan(pos, span)

    def findGlobal(x: Name): List[Span] = value.findGlobal(x)

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

    def spanOf(pos: PosInfo)(implicit
        env: Map[Name, Span]
    ): Option[List[Span]] =
      inline def go(x: Name, span: Span) =
        if posInSpan(pos, span) then env.get(x).map(List(_)) else None
      inline def to(l: List[Span]): Option[List[Span]] = l match
        case Nil => None
        case l   => Some(l)
      this match
        case Var(x, ty, span)    => go(x, span)
        case Global(x, ty, span) => go(x, span)
        case NatLit(v, span)     => None
        case BoolLit(v, span)    => None
        case Succ(span)          => None
        case App(fn, arg)        => fn.spanOf(pos).orElse(arg.spanOf(pos))
        case If(span, rty, c, t, f) =>
          c.spanOf(pos).orElse(t.spanOf(pos)).orElse(f.spanOf(pos))
        case Iterate(span, rty, n, z, s) =>
          n.spanOf(pos).orElse(z.spanOf(pos)).orElse(s.spanOf(pos))
        case Lam(x, span, ty, b) =>
          if posInSpan(pos, span) then to(b.findVar(x))
          else b.spanOf(pos)(env + (x -> span))
        case Let(x, span, ty, v, b) =>
          if posInSpan(pos, span) then to(b.findVar(x))
          else v.spanOf(pos).orElse(b.spanOf(pos)(env + (x -> span)))

    def findVar(x: Name): List[Span] = this match
      case Var(y, _, span)    => if x == y then List(span) else Nil
      case Global(y, _, span) => Nil
      case NatLit(_, span)    => Nil
      case BoolLit(_, span)   => Nil
      case Succ(_)            => Nil
      case Lam(y, _, _, b)    => if x == y then Nil else b.findVar(x)
      case App(fn, arg)       => fn.findVar(x) ++ arg.findVar(x)
      case Let(y, _, _, v, b) =>
        v.findVar(x) ++ (if x == y then Nil else b.findVar(x))
      case If(_, _, c, t, f) => c.findVar(x) ++ t.findVar(x) ++ f.findVar(x)
      case Iterate(_, _, n, z, s) =>
        n.findVar(x) ++ z.findVar(x) ++ s.findVar(x)

    def findGlobal(x: Name): List[Span] = this match
      case Var(y, _, span)    => Nil
      case Global(y, _, span) => if x == y then List(span) else Nil
      case NatLit(_, span)    => Nil
      case BoolLit(_, span)   => Nil
      case Succ(_)            => Nil
      case Lam(y, _, _, b)    => if x == y then Nil else b.findGlobal(x)
      case App(fn, arg)       => fn.findGlobal(x) ++ arg.findGlobal(x)
      case Let(y, _, _, v, b) =>
        v.findGlobal(x) ++ (if x == y then Nil else b.findGlobal(x))
      case If(_, _, c, t, f) =>
        c.findGlobal(x) ++ t.findGlobal(x) ++ f.findGlobal(x)
      case Iterate(_, _, n, z, s) =>
        n.findGlobal(x) ++ z.findGlobal(x) ++ s.findGlobal(x)
  export Tm.*
