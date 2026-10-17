// Small JSON bridge to Claude Agent SDK's native session APIs.
//
// The SDK is resolved from the directory named by ROSTER_CLAUDE_SDK_DIR,
// where `roster-claude-install-sdk' installs it.

import { createReadStream, readdirSync } from "node:fs";
import { createRequire } from "node:module";
import { homedir } from "node:os";
import { basename, join } from "node:path";
import { pathToFileURL } from "node:url";

const SDK_PACKAGE = "@anthropic-ai/claude-agent-sdk";

// Value of a transcript's `entrypoint' field for interactive CLI sessions.
// Headless Agent SDK runs record another value (e.g. "sdk-ts") and cannot be
// resumed with `claude -r'.  `listSessions' does not expose this field.
const CLI_ENTRYPOINT = "cli";
const ENTRYPOINT_READ_SIZE = 65536;

function emit(value) {
  process.stdout.write(JSON.stringify(value) + "\n");
}

async function loadSdk() {
  const base = process.env.ROSTER_CLAUDE_SDK_DIR || process.cwd();
  try {
    const require = createRequire(join(base, "package.json"));
    return await import(pathToFileURL(require.resolve(SDK_PACKAGE)).href);
  } catch {
    return null;
  }
}

function projectsDir() {
  const configDir = process.env.CLAUDE_CONFIG_DIR || join(homedir(), ".claude");
  return join(configDir, "projects");
}

// Return a session-id-to-path map of top-level transcripts.  The scan is
// deliberately one level deep: subagent transcripts live under
// `<session-id>/subagents/' and are not sessions.
function transcriptPaths() {
  const paths = new Map();
  let projects;
  try {
    projects = readdirSync(projectsDir(), { withFileTypes: true });
  } catch {
    return paths;
  }
  for (const project of projects) {
    if (!project.isDirectory()) continue;
    const dir = join(projectsDir(), project.name);
    for (const file of readdirSync(dir, { withFileTypes: true })) {
      if (file.isFile() && file.name.endsWith(".jsonl")) {
        paths.set(basename(file.name, ".jsonl"), join(dir, file.name));
      }
    }
  }
  return paths;
}

// Return the first `entrypoint' recorded near the start of PATH, or null.
async function transcriptEntrypoint(path) {
  let head = "";
  try {
    for await (const chunk of createReadStream(path, {
      encoding: "utf8",
      end: ENTRYPOINT_READ_SIZE - 1,
    })) {
      head += chunk;
    }
  } catch {
    return null;
  }
  for (const line of head.split("\n")) {
    try {
      const entrypoint = JSON.parse(line)?.entrypoint;
      if (typeof entrypoint === "string" && entrypoint !== "") return entrypoint;
    } catch {
      // Ignore malformed lines and the partial line at the read boundary.
    }
  }
  return null;
}

// Return whether SESSION was started by the interactive CLI.  Transcripts
// predating the `entrypoint' field record nothing and are kept.
async function resumable(session, paths) {
  const path = paths.get(session.sessionId);
  const entrypoint = path ? await transcriptEntrypoint(path) : null;
  return entrypoint === null || entrypoint === CLI_ENTRYPOINT;
}

function sessionValue(session) {
  return {
    id: session.sessionId,
    title: session.summary,
    directory: session.cwd ?? null,
    time_updated: session.lastModified,
  };
}

const sdk = await loadSdk();
if (!sdk) {
  emit({ available: false });
  process.exit(0);
}

try {
  const [command, ...args] = process.argv.slice(2);
  if (command === "list") {
    const paths = transcriptPaths();
    const sessions = [];
    for (const session of await sdk.listSessions()) {
      if (await resumable(session, paths)) sessions.push(sessionValue(session));
    }
    emit({ available: true, sessions });
  } else if (command === "rename") {
    await sdk.renameSession(args[0], args[1]);
    emit({ available: true });
  } else if (command === "delete") {
    await sdk.deleteSession(args[0]);
    emit({ available: true });
  } else {
    throw new Error(`unknown command: ${command}`);
  }
} catch (error) {
  // Surface SDK errors as structured protocol failures.
  emit({ available: true, error: String(error?.message ?? error) });
  process.exit(1);
}
