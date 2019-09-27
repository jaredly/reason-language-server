// @flow
import { ExtensionContext, LanguageClient, services, workspace } from 'coc.nvim';
import fs from 'fs';
import os from 'os';
import axios from 'axios';
import AdmZip from 'adm-zip';

const RELEASE_URL = 'https://github.com/jaredly/reason-language-server/releases/download';

const rlsDownloadUrl = (version: string, OS: string) => `${RELEASE_URL}/${version}/rls-${OS}.zip`;

const getOS = () => {
  switch (os.platform()) {
    case 'linux':
      return 'linux';
    case 'darwin':
      return 'macos';
    case 'win32':
      return 'windows';
    default:
      throw new Error(`Platform ${os.platform()} is not supported!`);
  }
};

async function getCommand(context: ExtensionContext): Promise<string> {
  const currentVersion = fs.readFileSync(context.asAbsolutePath('./rls/CURRENT'), 'UTF-8').trim();
  const OS = getOS();

  const fileExt = OS === 'windows' ? '.exe' : '';

  const cmdPath = context.asAbsolutePath(`./rls/rls-${OS}/reason-language-server${fileExt}`);
  if (fs.existsSync(cmdPath)) {
    return cmdPath;
  }

  workspace.showMessage(`Downloading reason-language-server for ${OS}...`);
  const { data } = await axios.get(rlsDownloadUrl(currentVersion, OS), { responseType: 'arraybuffer' });
  const zipPath = context.asAbsolutePath(`./rls/rls-${OS}.zip`);
  fs.writeFileSync(zipPath, data);

  const zip = new AdmZip(zipPath);
  zip.extractAllTo(context.asAbsolutePath('./rls'));
  if (OS !== 'windows') {
    fs.chmodSync(cmdPath, '755');
  }

  workspace.showMessage(`Installed reason-language-server for ${OS}!`);
  return cmdPath;
}

async function activate(context: ExtensionContext) {
  const command = await getCommand(context);

  const serverOptions = { command };

  const documentSelector = [{ language: 'reason', scheme: 'file' }];
  const clientOptions = { documentSelector };

  const languageClient = new LanguageClient('reason', 'ReasonML', serverOptions, clientOptions);
  context.subscriptions.push(services.registLanguageClient(languageClient));
}

export { activate };
