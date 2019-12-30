#!/usr/bin/env node

const fs = require('fs');
const { spawnSync } = require('child_process');

const TRIGGER = 'actions: release';

const packageName = require('../../editor-extensions/vscode/package.json').name;

const getMostRecentVersion = () => {
    const result = spawnSync('npx', ['vsce', 'show', '--json', 'jaredly.reason-vscode'], {
        cwd: __dirname,
        encoding: 'utf8',
        shell: true,
    });
    if (result.status !== 0) {
        throw new Error('Unable to get most recent version of ' + packageName);
    }
    const data = JSON.parse(result.stdout.trim());
    const latestVersion = data.versions[0].version
    return latestVersion
};

const setFailed = message => {
    process.exitCode = 1;
    console.log('::error::' + message);
};

async function run() {
    const payload = JSON.parse(
        fs.readFileSync(process.env.GITHUB_EVENT_PATH, { encoding: 'utf8' }),
    );
    const match = payload.head_commit.message.split('\n')[0].trim() === TRIGGER;

    if (!match) {
        setFailed('No trigger');
        return;
    }

    const mostRecentVersion = getMostRecentVersion();
    const nextVersion = require('../../editor-extensions/vscode/package.json').version;
    if (nextVersion === mostRecentVersion) {
        setFailed('Version is the same as most recent');
        return;
    }
}

run().catch(err => {
    console.error(err);
    setFailed('Unexpected error');
});
