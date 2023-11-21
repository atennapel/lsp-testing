import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.TextDocumentService
import java.util
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.*

class LangTextDocumentService(langServer: LangLanguageServer) extends TextDocumentService:
  private val logger = Logger.instance

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    val uri = params.getTextDocument.getUri
    logger.log(s"didOpen $uri")
    val pos1 = new Position(0, 0)
    val pos2 = new Position(0, 10)
    val range = new Range(pos1, pos2)
    val diag = new Diagnostic(range, "Oh no!")
    val diags = new PublishDiagnosticsParams(uri, List(diag).asJava)
    langServer.getClient.publishDiagnostics(diags)

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    logger.log(s"didChange ${params.getTextDocument.getUri}")

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    logger.log(s"didClose ${params.getTextDocument.getUri}")

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    logger.log(s"didSave ${params.getTextDocument.getUri}")

  override def completion(position: CompletionParams): CompletableFuture[Either[util.List[CompletionItem], CompletionList]] =
    CompletableFuture.supplyAsync { () =>
      logger.log("completion")
      val item = new CompletionItem
      item.setLabel("Test completion")
      item.setInsertText("Blablabla")
      item.setDetail("Detail")
      item.setKind(CompletionItemKind.Snippet)
      Either.forLeft(List(item).asJava)
    }
