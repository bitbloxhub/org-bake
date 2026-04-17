# AGENTS.md

## Project summary

`org-bake` is an async-first Org indexing system.

Current project rules:
- Use `ox-json` as only exporter
- Stored document JSON uses top-level `document` field for exported Org AST directly
- Do not split files unless explicitly requested
- Prefer preserving existing implementation details when extending behavior
- Indexing runs in background and reacts to filesystem changes
- Materializations are custom, async, and built from stored document JSON only
- Built-in `tags` materializer lives in `org-bake-materialize.el`

## Elisp formatting

Format all Elisp files with `elisp-autofmt-buffer`.

Command:

```bash
XDG_CONFIG_HOME="$(mktemp -d)" XDG_CACHE_HOME="$(mktemp -d)" XDG_STATE_HOME="$(mktemp -d)" \
nix develop -c emacs --batch \
  --eval '(setq user-emacs-directory (make-temp-file "emacs-user-dir-" t))' \
  --eval '(require (quote elisp-autofmt))' \
  --eval '(let ((files (if (executable-find "fd")
                           (process-lines "fd" "-e" "el" ".")
                         (directory-files-recursively "." "\\.el\\'"))))
            (dolist (file files)
              (with-current-buffer (find-file-noselect file)
                (elisp-autofmt-buffer)
                (save-buffer))))'
```

## Paren check

Check all Elisp files for balanced parens:

```bash
XDG_CONFIG_HOME="$(mktemp -d)" XDG_CACHE_HOME="$(mktemp -d)" XDG_STATE_HOME="$(mktemp -d)" \
nix develop -c emacs --batch \
  --eval '(setq user-emacs-directory (make-temp-file "emacs-user-dir-" t))' \
  --eval '(let ((files (if (executable-find "fd")
                           (process-lines "fd" "-e" "el" ".")
                         (directory-files-recursively "." "\\.el\\'"))))
            (dolist (file files)
              (with-temp-buffer
                (insert-file-contents file)
                (check-parens))))'
```

## Byte compile smoke test

```bash
nix develop -c emacs --batch -Q -L . \
  -f batch-byte-compile org-bake.el org-bake-project.el org-bake-store.el org-bake-process.el org-bake-export.el org-bake-materialize.el
```

## Test workspace

Test fixture paths:
- `tests/org/`
- `tests/init-dir/init.el`

Example scan:

```bash
nix develop -c emacs --batch -Q -l tests/init-dir/init.el \
  --eval '(prin1 (length (org-bake-scan-workspace (quote test))))'
```

Example force rebake:

```bash
nix develop -c emacs --batch -Q -l tests/init-dir/init.el \
  --eval '(org-bake-rebake-workspace (quote test))'
```

Example rematerialize:

```bash
nix develop -c emacs --batch -Q -l tests/init-dir/init.el \
  --eval '(org-bake-rematerialize-workspace (quote test))'
```

## Nix / tools

Dev shell should include:
- Emacs with `async`
- Emacs with `ox-json`
- Emacs with `elisp-autofmt`

## Implementation notes

- Workspace storage defaults to XDG data dir
- Background indexing should use directory watches for create/delete/change events
- Keep public docstrings accurate when behavior changes
- Materializers register through `org-bake-register-materializer`
- Materializer builders must be named function symbols loadable in child Emacs
- Materialization outputs live under `materializations/<name>-<version>.json`
- Materializations currently rebuild on every relevant document change/delete
- Keep `AGENTS.md` commands in sync with project files and workflow
