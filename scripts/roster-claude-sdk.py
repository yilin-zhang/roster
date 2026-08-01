#!/usr/bin/env python3
"""Small JSON bridge to Claude Agent SDK's native session APIs."""

import json
import sys


def emit(value):
    print(json.dumps(value, ensure_ascii=False, separators=(",", ":")))


try:
    from claude_agent_sdk import list_sessions, rename_session
except ImportError:
    emit({"available": False})
    raise SystemExit(0)


def session_value(session):
    return {
        "id": session.session_id,
        "title": session.summary,
        "directory": session.cwd,
        "time_updated": session.last_modified,
    }


try:
    command = sys.argv[1]
    if command == "list":
        emit({"available": True, "sessions": [session_value(x) for x in list_sessions()]})
    elif command == "rename":
        rename_session(sys.argv[2], sys.argv[3], directory=sys.argv[4])
        emit({"available": True})
    else:
        raise ValueError(f"unknown command: {command}")
except Exception as error:  # Surface SDK errors as structured protocol failures.
    emit({"available": True, "error": str(error)})
    raise SystemExit(1)
