# ocman

`ocman` is a small Emacs helper for managing OpenCode and Claude Code sessions.

## Features

- Resume any existing OpenCode or Claude Code session
- Resume the latest session in the current project
- Browse sessions in a Dired-like management buffer
- Rename sessions
- Archive and unarchive sessions
- Delete sessions from either backend
- Safely move OpenCode sessions to another directory while preserving project resolution

## Commands

- `ocman-open-session`
- `ocman-open-session-project`
- `ocman-open-latest-session-project`
- `ocman-list-sessions`
- `ocman-list-project-sessions`
- `ocman-rename-session`
- `ocman-archive-session`
- `ocman-unarchive-session`
- `ocman-delete-session`
- `ocman-update-session-directory`

## Session List UI

`ocman-list-sessions` opens a tabulated management buffer for root sessions.
`ocman-list-project-sessions` narrows that view to the current project scope.

The list shows an `OC` or `CC` tag for each session.

- `RET` / `e`: resume session
- `d`: delete session
- `r`: rename session
- `a`: archive or unarchive session
- `R`: move session to another known project directory
- `o`: open session directory in Dired
- `c`: create a new session
- `t`: toggle archived sessions
- `g`: refresh
- `q`: quit the window

## Example config

```elisp
(use-package ocman
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/ocman/"
  :commands (ocman-open-session
             ocman-open-session-project
             ocman-list-sessions
             ocman-list-project-sessions
             ocman-open-latest-session-project
             ocman-rename-session
             ocman-archive-session
             ocman-unarchive-session
             ocman-delete-session
             ocman-update-session-directory)
  :custom
  (ocman-enabled-tools '(opencode claude))
  (ocman-default-new-session-tool 'opencode)
  (ocman-terminal-function (if (eq system-type 'darwin)
                               #'ocman-open-in-ghostty
                             #'ocman-open-in-emacs-terminal)))
```

If you prefer iTerm on macOS instead:

```elisp
(setq ocman-terminal-function #'ocman-open-in-iterm)
```

On macOS, both `ocman-open-in-ghostty` and `ocman-open-in-iterm` open a new tab
and start `opencode` there.

## Notes

- OpenCode session deletion uses `opencode session delete`; Claude Code deletion removes the local JSONL session file.
- OpenCode rename and archive operations currently update the local OpenCode database directly to match OpenCode's internal fields.
- Claude Code rename and archive state are stored in small `*.ocman.json` sidecar files next to the session JSONL files.
- Directory moves are OpenCode-only.
- Normal listings intentionally hide child/subagent sessions and only show root sessions.
