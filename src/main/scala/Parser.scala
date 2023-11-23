import Surface.*

import parsley.Parsley, Parsley.*
import scala.language.implicitConversions

object Parser:
  object LangLexer:
    import parsley.token.{LanguageDef, Lexer, Predicate, Parser}
    import parsley.character.{alphaNum, isWhitespace, char, oneOf}
    import parsley.combinator.{eof, many}

    private val userOps = "`~!@$%^&*-+=|:/?><,."
    private val userOpsTail = s"$userOps#_;"

    private val lang = LanguageDef.plain.copy(
      commentLine = "--",
      commentStart = "{-",
      commentEnd = "-}",
      nestedComments = true,
      keywords = Set(
        "def",
        "let",
        "Nat",
        "Bool",
        "True",
        "False",
        "S",
        "if",
        "then",
        "else",
        "iterate"
      ),
      operators = Set(
        "=",
        ":",
        ";",
        "\\",
        "=>",
        "->"
      ),
      identStart = Predicate(c => c.isLetter || c == '_'),
      identLetter = Predicate(c =>
        c.isLetterOrDigit || c == '_' || c == '\'' || c == '-' || c == '/' || c == ':'
      ),
      opStart = Predicate(userOps.contains(_)),
      opLetter = Predicate(userOpsTail.contains(_)),
      space = Predicate(isWhitespace)
    )
    val lexer = new Lexer(lang)

    def fully[A](p: => Parsley[A]): Parsley[A] = lexer.whiteSpace *> p <* eof

    val ident: Parsley[String] = lexer.identifier
    val userOp: Parsley[String] = lexer.userOp
    val natural: Parsley[Int] = lexer.natural
    val string: Parsley[String] = lexer.stringLiteral
    val int: Parsley[Int] = lexer.integer

    object Implicits:
      given Conversion[String, Parsley[Unit]] with
        def apply(s: String): Parsley[Unit] =
          if lang.keywords(s) then lexer.keyword(s)
          else if lang.operators(s) then lexer.maxOp(s)
          else void(lexer.symbol_(s))

  object TmParser:
    import parsley.expr.{precedence, Ops, InfixL, InfixR, Prefix, Postfix}
    import parsley.combinator.{many, some, option, sepEndBy, sepBy}
    import parsley.Parsley.pos

    import LangLexer.{ident as ident0, userOp as userOp0, natural, string}
    import LangLexer.Implicits.given

    private def positioned(p: => Parsley[Tm]): Parsley[Tm] =
      (pos <~> p).map(Pos.apply)

    private def tpositioned(p: => Parsley[Ty]): Parsley[Ty] =
      (pos <~> p).map(TPos.apply)

    private lazy val ident: Parsley[Name] = ident0
    private lazy val userOp: Parsley[Name] = userOp0
    private lazy val identOrOp: Parsley[Name] = ("(" *> userOp <* ")") <|> ident
    private lazy val identOrOpWithPos: Parsley[(Name, PosInfo)] =
      (("(" *> (pos <~> userOp) <* ")") <|> (pos <~> ident)).map((p, x) =>
        (x, p)
      )

    private lazy val bind: Parsley[Name] = "_".map(_ => "_") <|> identOrOp
    private lazy val bindWithPos: Parsley[(Name, PosInfo)] =
      (pos <~> bind).map((p, x) => (x, p))

    private lazy val holeP: Parsley[Tm] =
      ident.flatMap(x0 => {
        val x = x0
        if x.head == '_' then
          pure(Hole(if x.size > 1 then Some(x.tail) else None))
        else empty
      })

    private lazy val atom: Parsley[Tm] = positioned(
      attempt("(" *> (pos <~> userOp).map((p, x) => Var(x, p)) <* ")")
        <|> ("(" *> tm <* ")")
        <|> attempt(holeP)
        <|> natural.map(NatLit.apply)
        <|> ("True" #> BoolLit(true))
        <|> ("False" #> BoolLit(false))
        <|> ("S" #> Succ)
        <|> (pos <~> ident).map((pos, x) => Var(x, pos))
    )

    private val hole = Hole(None)

    lazy val tm: Parsley[Tm] = positioned(
      let <|> lam <|> ifP <|> iterateP <|> app
    )

    lazy val tyAtom: Parsley[Ty] = tpositioned(
      ("(" *> ty <* ")") <|> ("Nat" #> TNat) <|> ("Bool" #> TBool)
    )

    lazy val ty: Parsley[Ty] =
      tpositioned(
        precedence[Ty](tyAtom)(
          Ops(InfixR)("->" #> ((l, r) => TFun(l, r)))
        )
      )

    private type DefParam = (List[(Name, PosInfo)], Option[Ty])
    private lazy val defParam: Parsley[DefParam] =
      ("(" *> some(bindWithPos) <~> ":" *> ty <* ")")
        .map { case (xs, ty) =>
          (xs, Some(ty))
        } <|> bindWithPos.map(e => (List(e), None))

    private lazy val let: Parsley[Tm] =
      positioned(
        ("let" *> identOrOpWithPos <~> many(
          defParam
        ) <~> option(
          ":" *> ty
        ) <~> "=" *> tm <~> ";" *> tm)
          .map { case (((((x, xpos), ps), ty), v), b) =>
            Let(
              x,
              xpos,
              ty.map(typeFromParams(ps, _)),
              lamFromDefParams(ps, v, ty.isEmpty),
              b
            )
          }
      )

    private lazy val lam: Parsley[Tm] =
      positioned(
        ("\\" *> many(defParam) <~> "=>" *> tm).map(lamFromLamParams)
      )

    private lazy val ifP: Parsley[Tm] = positioned(
      ("if" *> tm <~> "then" *> tm <~> "else" *> tm).map { case ((c, a), b) =>
        If(c, a, b)
      }
    )

    private lazy val iterateP: Parsley[Tm] = positioned(
      ("iterate" *> atom <~> atom <~> (lam <|> atom)).map { case ((n, z), s) =>
        Iterate(n, z, s)
      }
    )

    private lazy val app: Parsley[Tm] =
      precedence[Tm](appAtom <|> lam)(
        defaultOps(
          (pos, op, t) => App(Var(op, pos), t),
          (pos, op, l, r) => App(App(Var(op, pos), l), r)
        )*
      )

    private lazy val appAtom: Parsley[Tm] = positioned(
      (projAtom <~> many(arg) <~> option(lam))
        .map { case ((fn, args), opt) =>
          (args.flatten ++ opt)
            .foldLeft(fn) { case (fn, arg) => App(fn, arg) }
        }
    )

    private type Arg = Tm
    private lazy val arg: Parsley[List[Arg]] = projAtom.map(t => List(t))

    private lazy val projAtom: Parsley[Tm] = positioned(atom)

    private def typeFromParams(ps: List[DefParam], rt: Ty): Ty =
      ps.foldRight(rt) { case ((xs, ty), b) =>
        xs.foldRight(b)((x, b) => TFun(ty.getOrElse(THole), b))
      }

    private def lamFromDefParams(
        ps: List[DefParam],
        b: Tm,
        useTypes: Boolean
    ): Tm =
      ps.foldRight(b) { case ((xs, ty), b) =>
        xs.foldRight(b) { case ((x, pos), b) =>
          Lam(
            x,
            pos,
            if useTypes then Some(ty.getOrElse(THole)) else None,
            b
          )
        }
      }

    private def lamFromLamParams(ps: List[DefParam], b: Tm): Tm =
      ps.foldRight(b) { case ((xs, ty), b) =>
        xs.foldRight(b) { case ((x, p), b) => Lam(x, p, ty, b) }
      }

    // operators
    private def userOpStart(s: String): Parsley[String] =
      userOp0.filter(_.startsWith(s))

    private def opL[T](
        o: String,
        handle: (PosInfo, Name, T, T) => T
    ): Parsley[InfixL.Op[T]] =
      attempt(pos <~> userOpStart(o).filterNot(_.endsWith(":"))).map((p, op) =>
        (l, r) => handle(p, op, l, r)
      )

    private def opR[T](
        o: String,
        handle: (PosInfo, Name, T, T) => T
    ): Parsley[InfixR.Op[T]] =
      attempt(pos <~> userOpStart(o)).map((p, op) =>
        (l, r) => handle(p, op, l, r)
      )

    private def opP[T](
        o: String,
        handle: (PosInfo, Name, T) => T
    ): Parsley[Prefix.Op[T]] =
      attempt(pos <~> userOpStart(o)).map((p, op) => t => handle(p, op, t))

    private def opLevel[T](
        s: String,
        prefix: (PosInfo, Name, T) => T,
        infix: (PosInfo, Name, T, T) => T
    ): List[Ops[T, T]] =
      val chars = s.toList
      List(
        Ops(Prefix)(chars.map(c => opP(c.toString, prefix))*),
        Ops(InfixL)(chars.map(c => opL(c.toString, infix))*),
        Ops(InfixR)(chars.map(c => opR(c.toString, infix))*)
      )

    private def ops[T](
        prefix: (PosInfo, Name, T) => T,
        infix: (PosInfo, Name, T, T) => T
    )(
        ss: String*
    ): List[Ops[T, T]] = ss.flatMap(s => opLevel(s, prefix, infix)).toList

    private def defaultOps[T](
        prefix: (PosInfo, Name, T) => T,
        infix: (PosInfo, Name, T, T) => T
    ): List[Ops[T, T]] =
      ops(prefix, infix)(
        "`@#?,.",
        "*/%",
        "+-",
        ":",
        "=!",
        "<>",
        "&",
        "^",
        "|",
        "$",
        "~"
      )

    // definitions
    lazy val defs: Parsley[List[Def]] = many(defP)

    private lazy val defP: Parsley[Def] =
      (pos <~> "def" *> identOrOpWithPos <~> many(
        defParam
      ) <~> option(
        ":" *> ty
      ) <~> "=" *> tm)
        .map { case ((((pos, (x, xpos)), ps), ty), v) =>
          Def(
            pos,
            x,
            xpos,
            ty.map(typeFromParams(ps, _)),
            lamFromDefParams(ps, v, ty.isEmpty)
          )
        }

  lazy val parser: Parsley[Tm] = LangLexer.fully(TmParser.tm)
  lazy val defsParser: Parsley[List[Def]] = LangLexer.fully(TmParser.defs)
