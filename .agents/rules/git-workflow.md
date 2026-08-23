---
trigger: always_on
---

# Git workflow

- Never commit or push unless the user explicitly asks in that moment. Approval for one commit/push does not carry over to later ones.
- Never run destructive or history-rewriting git commands (`reset --hard`, `push --force`, `checkout --`/`restore`/`clean -f` over uncommitted work, `branch -D`, amending a commit that's already pushed) unless the user explicitly requests it.
- Never skip hooks (`--no-verify`) or bypass signing (`--no-gpg-sign`) unless the user explicitly requests it.
- Before any command that could discard uncommitted work, run `git status` first; stash or commit anything unexpected rather than discarding it.
- When staging, check `git status`/`git diff` for the actual set of changed files — don't blind-stage with `git add -A`/`git add .`, and double-check file contents (not just filenames) for anything that looks like a secret before committing.
- Group commits by logical concern (fix / tests / docs / release metadata) rather than bundling unrelated changes into one commit.
- Prefer creating a new commit over amending an existing one, unless explicitly asked to amend.

## Commit message format

`type: imperative summary` in English, optionally `type(scope): summary` — e.g. `fix(run_did): report the estimation sample, not nrow(data)`. Common types used in this repo: `feat`, `fix`, `docs`, `test`, `ci`, `refactor`, `build`, `chore`. This mirrors the convention documented in `@/CLAUDE.md`.
