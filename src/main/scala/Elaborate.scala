import Surface as S
import Core.*

import scala.collection.mutable

object Elaborate:
  final case class Ctx(pos: S.PosInfo, env: Map[Name, Ty]):
    def enter(pos: S.PosInfo) = Ctx(pos, env)
    def bind(x: Name, ty: Ty) = Ctx(pos, env + (x -> ty))
    def lookup(x: Name): Option[Ty] = env.get(x)

  class ElaborateError(val ctx: Ctx, msg: String) extends Exception(msg):
    override def toString: String = s"ElaborateError(${ctx.pos}): $msg"

  private val globals: mutable.Map[String, Ty] = mutable.Map.empty

  private def err(msg: String)(implicit ctx: Ctx) =
    throw ElaborateError(ctx, msg)

  private def checkTy(ty: S.Ty)(implicit ctx: Ctx): Ty = ty match
    case S.TPos(pos, ty)  => checkTy(ty)(ctx.enter(pos))
    case S.TNat           => TNat
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
    case (S.Lam(x, None, b), TFun(pty, rty)) =>
      val eb = check(b, rty)(ctx.bind(x, pty))
      Lam(x, pty, eb)
    case (S.Let(x, t, v, b), _) =>
      val (ev, vt) = checkOrInfer(v, t)
      val eb = check(b, ty)(ctx.bind(x, vt))
      Let(x, vt, ev, eb)
    case _ =>
      val (etm, ety) = infer(tm)
      if ety != ty then err(s"type mismatch, expected $ty but got $ety")
      etm

  private def infer(tm: S.Tm)(implicit ctx: Ctx): (Tm, Ty) = tm match
    case S.Pos(pos, tm) => infer(tm)(ctx.enter(pos))
    case S.NatLit(v)    => (NatLit(v), TNat)
    case S.Hole(_)      => err(s"cannot infer hole")
    case S.Var(x) =>
      ctx.lookup(x) match
        case Some(ty) => (Var(x), ty)
        case None =>
          globals.get(x) match
            case Some(ty) => (Global(x), ty)
            case None     => err(s"undefined variable $x")
    case S.Lam(x, ty, b) =>
      ty match
        case None => err(s"cannot infer unannotated lambda")
        case Some(ty) =>
          val pty = checkTy(ty)
          val (ebody, rty) = infer(b)(ctx.bind(x, pty))
          (Lam(x, pty, ebody), TFun(pty, rty))
    case S.App(f, a) =>
      val (ef, fty) = infer(f)
      fty match
        case TFun(pty, rty) =>
          val ea = check(a, pty)
          (App(ef, ea), rty)
        case _ => err(s"function type expected in application but got $fty")
    case S.Let(x, t, v, b) =>
      val (ev, vt) = checkOrInfer(v, t)
      val (eb, rt) = infer(b)(ctx.bind(x, vt))
      (Let(x, vt, ev, eb), rt)

  private def elaborate(d: S.Def): Def = d match
    case S.Def(pos, x, ty, v) =>
      implicit val ctx: Ctx = Ctx(d.pos, Map.empty)
      if globals.get(x).isDefined then err(s"duplicate definition $x")
      val (etm, ety) = checkOrInfer(v, ty)
      globals += (x -> ety)
      Def(x, ety, etm)

  def elaborate(d: List[S.Def]): List[Def] =
    globals.clear()
    d.map(elaborate)
