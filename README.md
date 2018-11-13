# oeis2

Haskell interface for [Online Encyclopedia of Integer Sequences](https://oeis.org/); homage to [oeis](http://hackage.haskell.org/package/oeis).

## Difference from  [oeis](http://hackage.haskell.org/package/oeis)

- Source data of OEIS.  
  : [oeis](http://hackage.haskell.org/package/oeis) use `fmt=text`, but this library use `fmt=json`.
- Possible to get all search results.
- Search functions from ID or sub-sequence are merged.

## Usage

If using Stack, add the following to `stack.yaml`.

```yaml
extra-deps:
- github: 23prime/oeis2
  commit: HEAD
```

And, add import statement.

```haskell
import Math.OEIS
```

- Get all search results from sub-sequence

    - If `n == 0`, you get all search results.

        ```haskell
        ghci>searchSeq (SubSeq [1,2,3,4]) 0
        [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,...
        ghci>length it
        53
        ```

    - Otherwise, you get first `n` search results.

        ```haskell
        ghci>searchSeq (SubSeq [1,2,3,4]) 17
        [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,8,9,
        ghci>length it
        17
        ```

- Get first few terms from sub-sequence

    ```haskell
    ghci>getSeqData (SubSeq [1,2,2,3,3,3,4,4,4,4])
    Just [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,...
    ```

-  Get Maple function from sub-sequence

    ```haskell
    ghci>maple <$> lookupSeq (SubSeq [1,2,2,3,3,3,4,4,4,4])
    Just ["A002024 := n-> ceil((sqrt(1+8*n)-1)/2); seq(A002024(n), n=1..100);"]
    ```

- If no search result

    ```haskell
    ghci>lookupSeq (ID "1145141919893")
    Nothing
    ```