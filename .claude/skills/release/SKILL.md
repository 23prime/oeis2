---
name: release
description: "Cut a new oeis2 release by bumping the version in a PR and pushing a tag. Use when the user asks to release, cut a release, ship, or publish a new version of oeis2."
---

# Release

Pushing a `v*` tag triggers `.github/workflows/release.yml`, which runs the tests, creates the GitHub Release, and uploads the package to Hackage. The Hackage upload waits for manual approval of the `hackage` environment, because a published version cannot be deleted.

## Step 0 — Sync main

```bash
git branch --show-current
git status
```

- Must be on `main` with a clean working tree. If not, confirm with the user how to proceed.

```bash
git pull origin main
```

## Step 1 — Propose a bump level

Get the version in `package.yaml` and the latest tag:

```bash
rg '^version:' package.yaml
git describe --tags --abbrev=0
```

If the latest tag is `v<version>`, take it as the base. Otherwise the tags lag behind the version, so take the last commit that changed `version:` in `package.yaml` as the base:

```bash
git log -1 --format=%H -G'^version:' -- package.yaml
```

List commits since the base, substituting the literal value in place of `<base>`:

```bash
git log <base>..HEAD --oneline --no-merges
```

Propose a level based on the changes:

| Changes since the base | Suggested level |
| --- | --- |
| Removed or changed exported API | `major` |
| Added exported API or modules | `minor` |
| Dependency bounds, bug fixes, internal changes, docs | `patch` |

Present the proposal with the reasoning to the user and get confirmation (or their own choice) before proceeding.

## Step 2 — Bump the version in a PR

`main` requires a review, so the bump goes through a PR. Create a branch `release-v<X.Y.Z>`, set `version` in `package.yaml` to the new version, and regenerate `oeis2.cabal`:

```bash
stack build
stack test
```

Commit both files and open a PR against `main`:

- Commit message: `Bump version: <old> -> <new>`
- PR title: `Release v<X.Y.Z>`

Wait for the checks:

```bash
gh pr checks <PR_NUMBER> --watch
```

If checks fail, report to the user and stop. Otherwise ask the user to review and merge the PR, and wait. Never merge it with admin privileges.

## Step 3 — Tag

After the PR is merged:

```bash
git checkout main
git pull origin main
rg '^version:' package.yaml
```

Confirm that `version` is the new one, then push an annotated tag:

```bash
git tag -a v<X.Y.Z> -m "v<X.Y.Z>"
git push origin v<X.Y.Z>
```

## Step 4 — Wait for the workflow

Get the run's database ID:

```bash
gh run list --workflow=release.yml --limit 1 --json databaseId,status,url
```

Tell the user that the `upload-hackage` job waits for approval of the `hackage` environment, and ask them to approve it. Then watch the run until it finishes, substituting the literal value in place of `<run_id>`:

```bash
gh run watch <run_id> --exit-status
```

If the command exits non-zero, inspect the failing job (`gh run view <run_id> --log-failed`) and resolve it before telling the user the release is done. Once it succeeds, report the run URL, the GitHub Release, and the Hackage page (`https://hackage.haskell.org/package/oeis2-<X.Y.Z>`).
