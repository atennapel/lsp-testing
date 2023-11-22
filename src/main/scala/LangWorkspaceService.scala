import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.WorkspaceService

class LangWorkspaceService(langServer: LangLanguageServer)
    extends WorkspaceService:
  private val logger = Logger.instance

  override def didChangeConfiguration(
      params: DidChangeConfigurationParams
  ): Unit =
    logger.log("didChangeConfiguration")

  override def didChangeWatchedFiles(
      params: DidChangeWatchedFilesParams
  ): Unit =
    logger.log("didChangeWatchedFiles")

  override def didRenameFiles(params: RenameFilesParams): Unit =
    logger.log("didRenameFiles")
