# Codex instructions

Before working in this repository, read and follow these files in order:

1. `../AGENTS.md` for the shared Codex adaptations.
2. `../CLAUDE.md` in full for the shared R working environment instructions.
3. This repository's `CLAUDE.md` in full, if present, for project-specific
   instructions and explicit overrides of the shared defaults.

Resolve these paths relative to this file, not the shell's current directory.
The shared directory is `C:/Rworking` on Windows, `/mnt/c/Rworking` in WSL,
and is also available through `/home/rfisher/Rworking_wsl` on this machine.
For a worktree, locate the shared files relative to the main checkout instead;
`git worktree list` identifies that checkout. If the shared files cannot be
found, report which files are missing before making changes that depend on them.

The Claude files are the authoritative shared instructions for both assistants.
Keep shared rules there so that Claude and Codex use the same guidance. Read
further instruction files when those documents require them for the task.
The explicit parent reads are required because Codex's automatic project
instruction discovery normally starts at the Git repository root.

This repository is the `bayesnec` R package. Apply the shared R-package rules
and read `DESCRIPTION` before changing package code or dependencies. No
repository-level `CLAUDE.md` was present when this file was added; its absence
does not prevent work under the shared instructions.

Apply instructions addressed to Claude to Codex where they describe development
behaviour. Attribute prompt-log entries to **Codex**, with the actual model
identifier when available. Preserve existing `<TOPIC>-claude.md` specification
filenames and Claude-directed comment markers.

Claude's hooks do not run automatically in Codex. After writing or editing a
`.md` or `.qmd` file, run the shared check explicitly when available:

```bash
python3 ~/.claude/hooks/prose-check.py <file>
```

Follow the shared prose review requirements as well. If a required check or
skill is unavailable, report that limitation instead of claiming it ran.
