A linear algebra library which provides
most functions taught in Linear Algebra
I and II.

Main library is stored in [./src/Vectorspace.hs](./src/Vectorspace.hs)

# How to

```bash
git clone https://github.com/Quoteme/vectorspace.git
stack repl
```

And you from here you can run all the examples

# Examples

```haskell
vecadd [1,2,3] [1,2,3] -- [2,4,6]
vecadd [1,2,3] -- (x -> x + [1,2,3])

transpose [[1,2,3],[4,5,6],[7,8,9]] -- [[1,4,7],[2,5,8],[3,6,9]]

inv [[1,2],[2,1]] -- Just [[-0.3333333333333333,0.6666666666666666],[0.6666666666666666,-0.3333333333333333]]
inv [[1,0],[2,0]] -- Nothing
```
