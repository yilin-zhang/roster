# Roster

Roster is an Emacs package for managing AI coding sessions from OpenCode,
Claude Code, Codex, and pi.

These tools accumulate sessions over time with no easy way to browse, resume, or
clean them up. Roster puts all sessions in a unified `tabulated-list-mode`
buffer — tagged `OC` for OpenCode, `CC` for Claude Code, `CX` for Codex, and
`PI` for pi — and lets you resume, rename, archive, delete, and move them
without leaving your editor.

## Install

```elisp
(use-package roster
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/roster/"
  :commands (roster roster-project)
  :custom
  ;; Omit `roster-enabled-tools' to enable all supported agents automatically.
  (roster-enabled-tools '(opencode claude codex pi))
  (roster-default-new-session-tool 'opencode)
  (roster-terminal-function #'roster-open-in-ghostty))
```

On macOS, `roster-open-in-ghostty` and `roster-open-in-iterm` are also
available. If you prefer iTerm:

```elisp
(setq roster-terminal-function #'roster-open-in-iterm)
```

## Usage

Two entry points open the session list:

- `roster` — full session list
- `roster-project` — sessions filtered to the current project

From the list buffer:

| Key         | Action                                                  |
|-------------|---------------------------------------------------------|
| `RET` / `e` | Resume session                                          |
| `r`         | Rename session                                          |
| `a`         | Archive/unarchive marked sessions (or session at point) |
| `d`         | Delete marked sessions (or session at point)            |
| `R`         | Move session to another project directory               |
| `o`         | Open session directory in Dired                         |
| `c`         | Create a new session                                    |
| `t`         | Toggle display of archived sessions                     |
| `g`         | Refresh                                                 |
| `q`         | Quit                                                    |
| `m`         | Mark session at point (or region)                       |
| `u`         | Unmark session at point                                 |
| `U`         | Unmark all                                              |

Directory moves (`R`) are only supported for OpenCode sessions.

## Structure

- `roster-core.el` — shared data model, customization, and terminal helpers
- `roster-{opencode,claude,codex,pi}.el` — isolated session backends
- `roster.el` — unified list UI, command dispatch, and public entry points
- `tests/roster-*-test.el` — backend- and UI-specific ERT suites

## Notes

- OpenCode sessions are read from and written to the OpenCode SQLite database
  directly.
- Claude Code sessions are read from JSONL files under `~/.claude/projects/`.
  Since Claude Code's database is not writable by third parties, custom titles
  and archive state are stored in small `~/.claude/roster/*.roster.json`
  sidecar files.
- Codex sessions are listed, renamed, archived, unarchived, and deleted through
  the official app-server API. Legacy roster sidecars remain readable only for
  migration. Codex filters subagent and guardian threads from its default
  interactive listing.
- pi sessions are read from JSONL files under `~/.pi/agent/sessions/`.
  Archived state is stored in `~/.pi/agent/roster/UUID.roster.json` sidecar
  files. Renames append `session_info` entries so pi itself sees the updated
  display name.
- Only root sessions are shown; child/subagent sessions are hidden.
- Requires Emacs 29.1+ for built-in SQLite support.
