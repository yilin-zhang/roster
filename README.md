# ocman

`ocman` is a small Emacs helper for managing OpenCode sessions.

## Features

- Resume any existing OpenCode session
- Resume the latest session in the current project
- Browse sessions in a Dired-like management buffer
- Rename sessions
- Archive and unarchive sessions
- Delete sessions via the official OpenCode CLI
- Safely move a session to another known OpenCode project directory

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
  (ocman-terminal-function (if (eq system-type 'darwin)
                               #'ocman-open-in-iterm
                             #'ocman-open-in-emacs-terminal)))
```

## Notes

- Session deletion uses `opencode session delete`.
- Rename and archive operations currently update the local OpenCode database directly to match OpenCode's internal fields.
- Directory moves are validated against known OpenCode projects before updating both `session.directory` and `session.project_id`.
- Normal listings intentionally hide child/subagent sessions and only show root sessions.
