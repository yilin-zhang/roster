// Small JSON bridge to Claude Agent SDK's native session APIs.
//
// The SDK is resolved from the directory named by ROSTER_CLAUDE_SDK_DIR,
// where `roster-claude-install-sdk' installs it.

import { execFileSync } from "node:child_process";
import {
  appendFileSync,
  createReadStream,
  readFileSync,
  readdirSync,
} from "node:fs";
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

function configDir() {
  return process.env.CLAUDE_CONFIG_DIR || join(homedir(), ".claude");
}

function projectsDir() {
  return join(configDir(), "projects");
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

// Return whether the process in RECORD, an entry of the CLI's
// `sessions/<pid>.json' registry, is still running.  The registry stores the
// process start time in UTC as `ps -o lstart' prints it, which tells a live
// CLI apart from an unrelated process that reused the pid of a crashed one.
function processRunning(record) {
  const pid = record.pid;
  if (!Number.isInteger(pid) || pid <= 0) return false;
  try {
    process.kill(pid, 0);
  } catch (error) {
    if (error.code !== "EPERM") return false;
  }
  if (typeof record.procStart !== "string") return true;
  const normalize = (time) => time.trim().replace(/\s+/g, " ");
  try {
    const started = execFileSync("ps", ["-o", "lstart=", "-p", String(pid)], {
      encoding: "utf8",
      env: { ...process.env, TZ: "UTC" },
      stdio: ["ignore", "pipe", "ignore"],
    });
    return normalize(started) === normalize(record.procStart);
  } catch (error) {
    // Without `ps' the start time cannot be checked; assume the pid is ours.
    return error.code === "ENOENT";
  }
}

// Return the pid of a running Claude Code process that has SESSION-ID open,
// or null.
function livePid(sessionId) {
  const dir = join(configDir(), "sessions");
  let files;
  try {
    files = readdirSync(dir);
  } catch {
    return null;
  }
  for (const name of files) {
    if (!name.endsWith(".json")) continue;
    let record;
    try {
      record = JSON.parse(readFileSync(join(dir, name), "utf8"));
    } catch {
      continue;
    }
    if (record?.sessionId === sessionId && processRunning(record)) {
      return record.pid;
    }
  }
  return null;
}

// Signal an error when a Claude Code process has SESSION-ID open.  ADVICE
// tells the user what to do instead.
function ensureClosed(sessionId, advice) {
  const pid = livePid(sessionId);
  if (pid !== null) {
    throw new Error(`session is open in Claude Code (pid ${pid}); ${advice}`);
  }
}

// Rename SESSION-ID the way the CLI's `/rename' does.  The SDK writes the
// `custom-title' entry; the CLI also records the name as `agent-name', which
// it shows inside a resumed session, so that entry is appended here.  A
// running CLI keeps its own name in memory and keeps rewriting it to the
// transcript, so an open session is refused instead.
async function rename(sessionId, title) {
  ensureClosed(sessionId, "use /rename there");
  await sdk.renameSession(sessionId, title);
  const path = transcriptPaths().get(sessionId);
  if (!path) throw new Error(`transcript of session ${sessionId} not found`);
  const entry = { type: "agent-name", agentName: title.trim(), sessionId };
  appendFileSync(path, JSON.stringify(entry) + "\n");
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
    await rename(args[0], args[1]);
    emit({ available: true });
  } else if (command === "delete") {
    // Deleting under a running CLI would leave it writing to a transcript
    // that no longer exists.
    ensureClosed(args[0], "exit it first");
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
