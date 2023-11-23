import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import java.util
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.collection.mutable

import Parser.defsParser
import Core.*
import Elaborate.elaborate
import scala.util.Success
import scala.util.Failure
import Elaborate.ElaborateError

class LangTextDocumentService(langServer: LangLanguageServer)
    extends TextDocumentService:
  private val logger = Logger.instance

  private final case class DocInfo(text: String, defs: Option[List[Def]])

  private val docMap: mutable.Map[String, DocInfo] = mutable.Map.empty

  private def parseAndElaborate(txt: String): Try[List[Def]] =
    defsParser.parse(txt).toTry.flatMap(ds => Try(elaborate(ds)))

  private def getPosFromParseError(msg: String): (Int, Int) =
    val dropped = msg.drop(13).takeWhile(_ != ')')
    val line = dropped.drop(5).takeWhile(_ != ',').trim
    val col = dropped.dropWhile(_ != ',').drop(9).trim
    (line.toInt - 1, col.toInt - 1)

  private def handleFile(uri: String, txt: String): Option[List[Def]] =
    parseAndElaborate(txt) match
      case Success(defs) =>
        logger.log(s"typecheck success: $defs")
        val diags = new PublishDiagnosticsParams(uri, List().asJava)
        langServer.getClient.publishDiagnostics(diags)
        Some(defs)
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
            val diags = new PublishDiagnosticsParams(uri, List(diag).asJava)
            langServer.getClient.publishDiagnostics(diags)
            None
          case _ => throw exc

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    val text = params.getTextDocument().getText()
    logger.log(s"didOpen $uri")
    val defs = handleFile(uri, text)
    docMap(uri) = DocInfo(text, defs)

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
    val defs = handleFile(uri, newTxt)
    docMap(uri) = DocInfo(newTxt, defs)

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    logger.log(s"didClose ${params.getTextDocument.getUri}")
    docMap.remove(params.getTextDocument().getUri())

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
      val info = docMap(uri)
      info.defs match
        case None => null
        case Some(defs) =>
          val p = pos(params.getPosition)
          defs.map(_.typeAt(p)).find(_.isDefined) match
            case None => null
            case Some(ty) =>
              val h = new Hover
              h.setContents(
                new MarkupContent(MarkupKind.PLAINTEXT, ty.get.pretty)
              )
              h
    }

  inline private def pos(pos: Position): PosInfo =
    (pos.getLine() + 1, pos.getCharacter() + 1)
