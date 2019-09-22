// @flow
import { ExtensionContext, LanguageClient, services } from 'coc.nvim';

const RLS_COMMAND: string = process.env.RLS_COMMAND || 'reason-language-server';

const activate = (context: ExtensionContext) => {
  const command = RLS_COMMAND;
  const serverOptions = { command };

  const documentSelector = [{ language: 'reason', scheme: 'file' }];
  const clientOptions = { documentSelector };

  const languageClient = new LanguageClient('reason', 'ReasonML', serverOptions, clientOptions);
  context.subscriptions.push(services.registLanguageClient(languageClient));
};

export { activate };
