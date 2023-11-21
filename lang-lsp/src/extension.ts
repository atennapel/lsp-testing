import * as vscode from 'vscode';
import { extensionInstance } from './core/extension';

const outputChannel = vscode.window.createOutputChannel("Albert")

export function activate(context: vscode.ExtensionContext) {
	outputChannel.append("here")
	//Set the context of the extension instance
	extensionInstance.setContext(context);
	outputChannel.append("here")
	//Initialize the LS Client extension instance.
	outputChannel.append("here")
	extensionInstance.init().catch(error => {
		outputChannel.append("here")
		console.log(`Failed to activate lang extension. ${error}`);
	})
	outputChannel.append("here")
}

export function deactivate() {}
