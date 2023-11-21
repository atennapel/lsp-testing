import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.MessageParams
import org.eclipse.lsp4j.MessageType

class Logger private ():
  private var client: Option[LanguageClient] = None

  def initialize(newClient: LanguageClient): Unit =
    client match
      case None =>
        client = Some(newClient)
      case _ =>

  def log(msg: String): Unit =
    client.foreach(_.logMessage(new MessageParams(MessageType.Info, msg)))

object Logger:
  var staticInstance: Option[Logger] = None
  def instance: Logger =
    staticInstance match
      case None =>
        val logger = new Logger
        staticInstance = Some(logger)
        logger
      case Some(logger) => logger
