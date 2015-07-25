## Applicative type class

```haskell
class (Functor f) => Applicative where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
