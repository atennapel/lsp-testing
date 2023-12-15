import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import java.util
import java.util.concurrent.CompletableFuture
import java.{util => ju}
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.collection.mutable

import Parser.defsParser
import Core.*
import scala.util.Success
import scala.util.Failure
import Elaborate.ElaborateError
import scala.annotation.tailrec
import ModuleLoading.UriError

class LangTextDocumentService(langServer: LangLanguageServer)
    extends TextDocumentService:
  private val logger = Logger.instance

  private final case class DocInfo(text: String, defs: Option[List[Def]])

  private def getPosFromParseError(msg: String): (Int, Int) =
    val dropped = msg.drop(13).takeWhile(_ != ')')
    val line = dropped.drop(5).takeWhile(_ != ',').trim
    val col = dropped.dropWhile(_ != ',').drop(9).trim
    (line.toInt - 1, col.toInt - 1)

  private def denormalizeURI(uri: String): String =
    val res = "file:/" + uri.drop(5).replace(":", "%3A").replace("\\", "/")
    logger.log(s"denormalize uri: $uri -> $res")
    res

  private def handleFile(uri: String, txt: Option[String]): Unit =
    logger.log(s"handleFile: $uri")
    val loaded = mutable.Set.empty[String]
    Try(ModuleLoading.load(uri, txt)(loaded)) match
      case Success(defs) =>
        logger.log(
          s"typecheck success: $defs, ${loaded.mkString("[", ",", "]")}"
        )
        loaded.foreach { uri =>
          val diags =
            new PublishDiagnosticsParams(denormalizeURI(uri), List().asJava)
          langServer.getClient.publishDiagnostics(diags)
        }
      case Failure(exc) =>
        exc match
          case err: Exception if err.getMessage.startsWith("ParseError:") =>
            val p = getPosFromParseError(err.getMessage)
            val pos = new Position(p._1, p._2)
            val range = new Range(pos, pos)
            val diag = new Diagnostic(range, err.getMessage)
            val diags = new PublishDiagnosticsParams(uri, List(diag).asJava)
            langServer.getClient.publishDiagnostics(diags)
            None
          case err: ElaborateError =>
            logger.log(err.toString)
            val pos = new Position(err.ctx.pos._1 - 1, err.ctx.pos._2 - 1)
            val range = new Range(pos, pos)
            val diag = new Diagnostic(range, err.getMessage)
            val diags = new PublishDiagnosticsParams(
              denormalizeURI(err.uri),
              List(diag).asJava
            )
            langServer.getClient.publishDiagnostics(diags)
            None
          case err: UriError =>
            val pos = err.pos match
              case Some((line, col)) => new Position(line - 1, col - 1)
              case None              => new Position(0, 0)
            val range = new Range(pos, pos)
            val diag = new Diagnostic(range, err.getMessage)
            val diags = new PublishDiagnosticsParams(uri, List(diag).asJava)
            langServer.getClient.publishDiagnostics(diags)
            None
          case err: Exception if err.getMessage.startsWith("cycle") =>
            val pos = new Position(0, 0)
            val range = new Range(pos, pos)
            val diag = new Diagnostic(range, err.getMessage)
            val diags = new PublishDiagnosticsParams(uri, List(diag).asJava)
            langServer.getClient.publishDiagnostics(diags)
            None
          case _ => throw exc

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    val text = params.getTextDocument().getText()
    logger.log(s"didOpen $uri")
    handleFile(uri, Some(text))

  private def getIx(s: String, line: Int, col: Int): Int =
    s.linesIterator.toList(line)(col)

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    logger.log(s"didChange $uri")
    /*
    val newTxt = params.getContentChanges().asScala.foldLeft(docMap(uri)) {
      (txt, change) =>
        val start = change.getRange().getStart()
        val end = change.getRange().getEnd()
        val startIx = getIx(txt, start.getLine(), start.getCharacter())
        val endIx = getIx(txt, end.getLine(), end.getCharacter())
        txt.patch(startIx, change.getText(), endIx - startIx)
    }
     */
    val newTxt = params.getContentChanges().get(0).getText()
    handleFile(uri, Some(newTxt))

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    logger.log(s"didClose ${params.getTextDocument.getUri}")

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    logger.log(s"didSave $uri")

  override def completion(
      position: CompletionParams
  ): CompletableFuture[Either[util.List[CompletionItem], CompletionList]] =
    CompletableFuture.supplyAsync { () =>
      logger.log("completion")
      val item = new CompletionItem
      item.setLabel("Test completion")
      item.setInsertText("Blablabla")
      item.setDetail("Detail")
      item.setKind(CompletionItemKind.Snippet)
      Either.forLeft(List(item).asJava)
    }

  override def hover(params: HoverParams): CompletableFuture[Hover] =
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument().getUri()
      logger.log(s"hover $uri")
      val defs = ModuleLoading.defs(uri)
      val p = pos(params.getPosition)
      firstMatch(defs, _.typeAt(p)) match
        case None => null
        case Some(ty) =>
          val h = new Hover
          h.setContents(
            new MarkupContent(MarkupKind.PLAINTEXT, ty.pretty)
          )
          h
    }

  inline private def pos(pos: Position): PosInfo =
    (pos.getLine() + 1, pos.getCharacter() + 1)
  def spanToRange(span: Span) =
    new Range(
      new Position(span._1._1 - 1, span._1._2 - 1),
      new Position(span._2._1 - 1, span._2._2 - 1)
    )

  override def definition(params: DefinitionParams): CompletableFuture[
    Either[ju.List[? <: Location], ju.List[? <: LocationLink]]
  ] =
    CompletableFuture.supplyAsync { () =>
      val uri = params.getTextDocument().getUri()
      logger.log(s"definition $uri")
      val defs = ModuleLoading.defs(uri)
      val p = pos(params.getPosition())
      defs.indexWhere(d => d.matchPos(p)) match
        case -1 =>
          val m = mutable.Map.empty[Name, Span]
          firstMatch(
            defs,
            d => {
              val res = d.spanOf(p)(m.toMap)
              m += (d.name -> d.span)
              res
            }
          ) match
            case None => null
            case Some(spans) =>
              Either.forLeft(
                spans
                  .map(span => new Location(uri, spanToRange(span)))
                  .asJava
              )
        case i =>
          val x = defs(i).name
          defs.drop(i + 1).flatMap(_.findGlobal(x)) match
            case Nil => null
            case spans =>
              Either.forLeft(
                spans
                  .map(span => new Location(uri, spanToRange(span)))
                  .asJava
              )
    }

  @tailrec
  private def firstMatch[A, B](l: List[A], f: A => Option[B]): Option[B] =
    l match
      case Nil => None
      case hd :: tl =>
        f(hd) match
          case None => firstMatch(tl, f)
          case res  => res
