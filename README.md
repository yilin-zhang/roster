# Roster

Roster is an Emacs package for managing AI coding sessions from OpenCode,
Claude Code, and Codex.

These tools accumulate sessions over time with no easy way to browse, resume, or
clean them up. Roster puts all sessions in a unified `tabulated-list-mode`
buffer — tagged `OC` for OpenCode, `CC` for Claude Code, and `CX` for Codex —
and lets you resume, rename, archive, delete, and move them without leaving your
editor.

## Install

```elisp
(use-package roster
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/roster/"
  :commands (roster-open-session
             roster-open-session-project
             roster-list-sessions
             roster-list-project-sessions
             roster-open-latest-session-project
             roster-rename-session
             roster-archive-session
             roster-unarchive-session
             roster-delete-session
             roster-update-session-directory)
  :custom
  (roster-enabled-tools '(opencode claude codex))
  (roster-default-new-session-tool 'opencode)
  (roster-terminal-function #'roster-open-in-ghostty))
```

On macOS, `roster-open-in-ghostty` and `roster-open-in-iterm` are also
available. If you prefer iTerm:

```elisp
(setq roster-terminal-function #'roster-open-in-iterm)
```

## Usage

### Quick commands

These work without opening the session list:

- `roster-open-session` — pick any session and resume it
- `roster-open-session-project` — pick a session scoped to the current project
- `roster-open-latest-session-project` — resume the most recent session in the current project
- `roster-list-sessions` — open the full session list
- `roster-list-project-sessions` — open the session list filtered to the current project

### Session list

`roster-list-sessions` opens a tabulated buffer showing all sessions. From
there:

| Key         | Action                                    |
|-------------|-------------------------------------------|
| `RET` / `e` | Resume session                            |
| `r`         | Rename session                            |
| `a`         | Archive or unarchive session              |
| `d`         | Delete session                            |
| `R`         | Move session to another project directory |
| `o`         | Open session directory in Dired           |
| `c`         | Create a new session                      |
| `t`         | Toggle display of archived sessions       |
| `g`         | Refresh                                   |
| `q`         | Quit                                      |
| `m`         | Mark session at point (or region)         |
| `u`         | Unmark session at point                   |
| `U`         | Unmark all                                |
| `D`         | Delete all marked sessions                |
| `A`         | Archive/unarchive all marked sessions     |

Directory moves (`R`) are only supported for OpenCode sessions.

## Notes

- OpenCode sessions are read from and written to the OpenCode SQLite database
  directly.
- Claude Code sessions are read from JSONL files under `~/.claude/projects/`.
  Since Claude Code's database is not writable by third parties, custom titles
  and archive state are stored in small `*.roster.json` sidecar files next to
  each session's JSONL file.
- Codex sessions are read from JSONL files under `~/.codex/sessions/YYYY/MM/DD/`.
  Archived sessions live in `~/.codex/archived_sessions/`; archiving and
  unarchiving moves files between the two directories. Custom titles are stored
  in `~/.codex/roster/UUID.roster.json` sidecar files.
- Only root sessions are shown; child/subagent sessions are hidden.
- Requires Emacs 29.1+ for built-in SQLite support.
