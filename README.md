# Roster

Roster is an Emacs interface for browsing and managing coding-agent sessions.
It brings OpenCode, Claude Code, Codex, and pi sessions into one searchable,
sortable list.

From that list you can resume, rename, archive, delete, or open a session's
working directory. Roster shows root sessions only, so Codex subagents and
other child sessions do not clutter the list.

## Requirements

- Emacs 29.1 or newer
- At least one supported coding agent installed and configured
- macOS when using the built-in Ghostty or iTerm launchers

## Installation

Clone this repository somewhere on your Emacs `load-path`, then configure it
with `use-package`:

```elisp
(use-package roster
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/roster/"
  :commands (roster roster-project))
```

Run `M-x roster` to open the full session list, or `M-x roster-project` to show
sessions belonging to the current project.

## Configuration

All supported tools are enabled by default. To select a subset or choose how
new sessions open:

```elisp
(use-package roster
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/roster/"
  :commands (roster roster-project)
  :custom
  (roster-enabled-tools '(opencode claude codex pi))
  (roster-default-new-session-tool 'opencode)
  (roster-terminal-function #'roster-open-in-ghostty))
```

On macOS, Roster includes launchers for Ghostty and iTerm:

```elisp
(setq roster-terminal-function #'roster-open-in-iterm)
```

Use `M-x customize-group RET roster` to see all available options, including
agent executable names and data locations.

## Commands

| Key         | Action                                                  |
|-------------|---------------------------------------------------------|
| `RET` / `e` | Resume session                                          |
| `r`         | Rename session                                          |
| `a`         | Archive/unarchive marked sessions (or session at point) |
| `d`         | Delete marked sessions (or session at point)            |
| `R`         | Move an OpenCode session to another project directory   |
| `o`         | Open session directory in Dired                         |
| `c`         | Create a new session                                    |
| `t`         | Toggle archived sessions                                |
| `m`         | Mark session at point or sessions in the active region  |
| `u`         | Unmark session at point                                 |
| `U`         | Unmark all sessions                                     |
| `g`         | Refresh                                                 |
| `q`         | Quit                                                    |

## Supported tools

| Tool        | Tag  | Resume | Rename | Archive | Delete | Move directory |
|-------------|------|--------|--------|---------|--------|----------------|
| OpenCode    | `OC` | Yes    | Yes    | Yes     | Yes    | Yes            |
| Claude Code | `CC` | Yes    | Yes    | Yes     | Yes    | No             |
| Codex       | `CX` | Yes    | Yes    | Yes     | Yes    | No             |
| pi          | `PI` | Yes    | Yes    | Yes     | Yes    | No             |

## Development

The package is split into a shared core, one backend module per supported tool,
and the list UI in `roster.el`. Backends register their display metadata and
supported operations through `roster-register-backend`, so the UI contains no
tool-specific dispatch. Tests follow the same backend/UI split under `tests/`.

Run the complete local check suite with:

```sh
./scripts/check-all.sh
```
