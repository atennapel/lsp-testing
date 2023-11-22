import org.eclipse.lsp4j.services.*
import org.eclipse.lsp4j.*
import java.util.concurrent.CompletableFuture
import java.util.UUID
import scala.jdk.CollectionConverters.*

class LangLanguageServer extends LanguageServer with LanguageClientAware:
  private val textDocumentService = new LangTextDocumentService(this)
  private val workspaceService = new LangWorkspaceService(this)

  private var clientCapabilities: Option[ClientCapabilities] = None
  private var client: Option[LanguageClient] = None

  private var shutdownStatus = 1

  def getClient: LanguageClient = client.get

  override def initialize(
      params: InitializeParams
  ): CompletableFuture[InitializeResult] =
    val response = new InitializeResult(new ServerCapabilities)
    response.getCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    response.getCapabilities().setHoverProvider(true)
    clientCapabilities = Some(params.getCapabilities)
    if !isDynamicCompletionRegistration then
      response.getCapabilities.setCompletionProvider(new CompletionOptions)
    CompletableFuture.supplyAsync(() => response)

  override def initialized(params: InitializedParams): Unit =
    if isDynamicCompletionRegistration then
      val options = new CompletionRegistrationOptions
      val registration = new Registration(
        UUID.randomUUID.toString,
        "textDocument/completion",
        options
      )
      client.foreach(
        _.registerCapability(new RegistrationParams(List(registration).asJava))
      )

  override def shutdown(): CompletableFuture[Object] =
    shutdownStatus = 0
    CompletableFuture.supplyAsync(() => new Object)

  override def exit(): Unit = System.exit(shutdownStatus)

  override def getTextDocumentService(): TextDocumentService =
    textDocumentService

  override def getWorkspaceService(): WorkspaceService = workspaceService

  override def connect(newClient: LanguageClient): Unit =
    client = Some(newClient)
    Logger.instance.initialize(newClient)

  private def isDynamicCompletionRegistration: Boolean =
    clientCapabilities match
      case None => false
      case Some(cap) =>
        val txt = cap.getTextDocument
        txt != null && txt.getCompletion != null && txt.getCompletion.getDynamicRegistration
