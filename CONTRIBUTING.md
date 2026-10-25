# Contributing

## Branches

- `main` is the only long-lived branch.
- Create a topic branch from `main` and open a pull request against `main`.

## Development

Build and run the tests with [Stack](https://docs.haskellstack.org/):

```sh
stack test
```

`oeis2.cabal` is generated from `package.yaml` by hpack, which Stack runs automatically. Edit `package.yaml` and commit the regenerated `oeis2.cabal` together.

## Tests

The tests do not access oeis.org, because it rejects requests from GitHub Actions runners.

- HTTP requests go through `Fetcher` (`Math.OEIS.Http`). The public functions in `Math.OEIS` use `httpFetch`, and the `*With` functions in `Math.OEIS.Internal` (e.g. `lookupSeqWith`) take a `Fetcher`.
- The tests in `test/Spec.hs` supply a mock `Fetcher` that returns the JSON responses stored in `test/fixtures/`.
- To add a fixture, save the response of `https://oeis.org/search?fmt=json&q=<query>` and make the mock return it for that query.

## Release

1. Bump `version` in `package.yaml` and regenerate `oeis2.cabal`.
2. Merge the change into `main`.
3. Upload the package to Hackage.
4. Create a GitHub release with the release notes (they are linked from the package description).
