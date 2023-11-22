import path = require("path");
import * as vscode from "vscode";
import {
  LanguageClientOptions,
  RevealOutputChannelOn,
} from "vscode-languageclient";

import {
  LanguageClient,
  ServerOptions,
  State,
} from "vscode-languageclient/node";

const outputChannel = vscode.window.createOutputChannel("Lang");

export class LangExtension {
  private languageClient?: LanguageClient;
  private context?: vscode.ExtensionContext;

  setContext(context: vscode.ExtensionContext) {
    this.context = context;
  }

  async init(): Promise<void> {
    try {
      //Server options. LS client will use these options to start the LS.
      let serverOptions: ServerOptions = getServerOptions();

      //creating the language client.
      let clientId = "lang-vscode-lsclient";
      let clientName = "Lang LS Client";
      let clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "lang" }],
        outputChannel: outputChannel,
        revealOutputChannelOn: RevealOutputChannelOn.Never,
      };
      this.languageClient = new LanguageClient(
        clientId,
        clientName,
        serverOptions,
        clientOptions
      );

      const disposeDidChange = this.languageClient.onDidChangeState(
        (stateChangeEvent) => {
          if (stateChangeEvent.newState === State.Stopped) {
            vscode.window.showErrorMessage(
              "Failed to initialize the extension"
            );
          } else if (stateChangeEvent.newState === State.Running) {
            vscode.window.showInformationMessage(
              "Extension initialized successfully!"
            );
          }
        }
      );

      let disposable = this.languageClient.start();
      // this.context!.subscriptions.push(disposable);
    } catch (exception) {
      return Promise.reject("Extension error!");
    }
  }
}

//Create a command to be run to start the LS java process.
function getServerOptions() {
  //Change the project home accordingly.
  const PROJECT_HOME = "C:/Users/Albert.ten.Napel/git/lsp-testing";
  const LS_LIB = "target/scala-3.3.1/lsp-testing-assembly-0.1.0-SNAPSHOT.jar";
  const LS_HOME = path.join(PROJECT_HOME, LS_LIB);
  const JAVA_HOME = process.env.JAVA_HOME;

  let executable: string = path.join(String(JAVA_HOME), "bin", "java");
  let args: string[] = ["-cp", LS_HOME];

  let serverOptions: ServerOptions = {
    command: executable,
    args: [...args, "run"],
    options: {},
  };
  return serverOptions;
}

export const extensionInstance = new LangExtension();
