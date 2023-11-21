import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

object Main:
  @main def run(): Unit =
    val server = new LangLanguageServer
    val launcher = Launcher.createLauncher(server, classOf[LanguageClient], System.in, System.out)
    val client = launcher.getRemoteProxy
    server.connect(client)
    launcher.startListening().get
