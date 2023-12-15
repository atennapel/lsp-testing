import Surface as S
import Core.*
import ElabState as ES

import java.nio.file.Path

object Elaborate:
  final case class Ctx(uri: String, pos: S.PosInfo, env: Map[Name, Ty]):
    def enter(pos: S.PosInfo) = Ctx(uri, pos, env)
    def bind(x: Name, ty: Ty) = Ctx(uri, pos, env + (x -> ty))
    def lookup(x: Name): Option[Ty] = env.get(x)

  private inline def span(name: Name)(implicit ctx: Ctx): Span =
    (ctx.pos, (ctx.pos._1, ctx.pos._2 + name.size - 1))

  class ElaborateError(val ctx: Ctx, msg: String) extends Exception(msg):
    def uri = ctx.uri
    def pos = ctx.pos
    override def toString: String = s"ElaborateError($uri, $pos): $msg"

  private def err(msg: String)(implicit ctx: Ctx) =
    throw ElaborateError(ctx, msg)

  private def checkTy(ty: S.Ty)(implicit ctx: Ctx): Ty = ty match
    case S.TPos(pos, ty)  => checkTy(ty)(ctx.enter(pos))
    case S.TNat           => TNat
    case S.TBool          => TBool
    case S.TFun(pty, rty) => TFun(checkTy(pty), checkTy(rty))
    case S.THole          => err(s"holes are not supported in types")

  private def checkOrInfer(tm: S.Tm, ty: Option[S.Ty])(implicit
      ctx: Ctx
  ): (Tm, Ty) =
    ty match
      case Some(ty) =>
        val ety = checkTy(ty)
        (check(tm, ety), ety)
      case None => infer(tm)

  private def check(tm: S.Tm, ty: Ty)(implicit ctx: Ctx): Tm = (tm, ty) match
    case (S.Pos(pos, tm), _) => check(tm, ty)(ctx.enter(pos))
    case (S.Hole(_), _)      => err(s"hole with type ${ty.pretty}")
    case (S.Lam(x, xpos, None, b), TFun(pty, rty)) =>
      val eb = check(b, rty)(ctx.bind(x, pty))
      Lam(x, span(x)(ctx.enter(xpos)), pty, eb)
    case (S.Let(x, xpos, t, v, b), _) =>
      val (ev, vt) = checkOrInfer(v, t)
      val eb = check(b, ty)(ctx.bind(x, vt))
      Let(x, span(x)(ctx.enter(xpos)), vt, ev, eb)
    case (S.If(c, a, b), _) =>
      val ec = check(c, TBool)
      val ea = check(a, ty)
      val eb = check(b, ty)
      If(span("if"), ty, ec, ea, eb)
    case (S.Iterate(n, z, s), _) =>
      val en = check(n, TNat)
      val ez = check(z, ty)
      val es = check(s, TFun(TNat, TFun(ty, ty)))
      Iterate(span("iterate"), ty, en, ez, es)
    case _ =>
      val (etm, ety) = infer(tm)
      if ety != ty then
        err(s"type mismatch, expected ${ty.pretty} but got ${ety.pretty}")
      etm

  private def infer(tm: S.Tm)(implicit ctx: Ctx): (Tm, Ty) = tm match
    case S.Pos(pos, tm) => infer(tm)(ctx.enter(pos))
    case S.NatLit(v) =>
      if v < 0 then err(s"negative numbers are not supported")
      else (NatLit(v, span(v.toString)), TNat)
    case S.BoolLit(b) => (BoolLit(b, span(b.toString)), TBool)
    case S.Succ       => (Succ(span("S")), TFun(TNat, TNat))
    case S.Hole(_)    => err(s"cannot infer hole")
    case S.Var(x, xpos) =>
      ctx.lookup(x) match
        case Some(ty) => (Var(x, ty, span(x)(ctx.enter(xpos))), ty)
        case None =>
          ES.lookup(x) match
            case Some(ty) => (Global(x, ty, span(x)(ctx.enter(xpos))), ty)
            case None     => err(s"undefined variable $x")
    case S.Lam(x, xpos, ty, b) =>
      ty match
        case None => err(s"cannot infer unannotated lambda")
        case Some(ty) =>
          val pty = checkTy(ty)
          val (ebody, rty) = infer(b)(ctx.bind(x, pty))
          (Lam(x, span(x)(ctx.enter(xpos)), pty, ebody), TFun(pty, rty))
    case S.App(f, a) =>
      val (ef, fty) = infer(f)
      fty match
        case TFun(pty, rty) =>
          val ea = check(a, pty)
          (App(ef, ea), rty)
        case _ =>
          err(s"function type expected in application but got ${fty.pretty}")
    case S.Let(x, xpos, t, v, b) =>
      val (ev, vt) = checkOrInfer(v, t)
      val (eb, rt) = infer(b)(ctx.bind(x, vt))
      (Let(x, span(x)(ctx.enter(xpos)), vt, ev, eb), rt)
    case S.If(c, a, b) =>
      val ec = check(c, TBool)
      val (ea, rty) = infer(a)
      val eb = check(b, rty)
      (If(span("if"), rty, ec, ea, eb), rty)
    case S.Iterate(n, z, s) =>
      val en = check(n, TNat)
      val (ez, rty) = infer(z)
      val es = check(s, TFun(TNat, TFun(rty, rty)))
      (Iterate(span("iterate"), rty, en, ez, es), rty)

  private def elaborate(uri: String, d: S.Def): Unit = d match
    case S.DImport(pos, uri) =>
    case S.DDef(pos, x, xpos, ty, v) =>
      implicit val ctx: Ctx = Ctx(uri, pos, Map.empty)
      if ES.exists(x) then err(s"duplicate definition $x")
      val (etm, ety) = checkOrInfer(v, ty)
      val d = Def(x, span(x)(ctx.enter(xpos)), ety, etm)
      ES.addDef(uri, d)

  def elaborate(uri: String, d: List[S.Def]): Unit =
    d.foreach(elaborate(uri, _))
