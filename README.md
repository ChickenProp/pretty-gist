This is an exploration of a new-to-me approach to stringification.

The lowest-friction way to stringify things in Haskell is usually `show`. It
gives the user zero ability to control how the thing is rendered.

The
`[Show1](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Classes.html#t:Show1)`
and
`[Show2](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Classes.html#t:Show2)`
classes give some control, but only in limited ways. Importantly, they only work
on paramaterized values; you could use `Show2` to change how you render the keys
of a `Map Int String`, but not of an `IntMap String`.

Here are some use cases where I've wanted more control over rendering than I can
easily get:

* My test suites generate a complicated data structure. Most of the time, most
  of this data structure is irrelevant to the test failures and I don't want to
  see it.

* A complicated data structure contains JSON values, and I want them rendered as
  JSON rather than as a Haskell data type. Or it contains floating-point
  numbers, and I want them rounded to 3dp.

* A nested data structure might be cyclic. I want to only show three levels
  deep.

`pretty-gist` aims to enable stuff like this.

## Design goals

* As a user, you should be able to target configurations specifically or
  generally. "All lists" or "all lists of Ints" or "only lists found at this
  specific point in the data structure". "All floating-point numbers" or
  "`Float` but not `Double`".

* It should pretty-print, with indentation that adjusts to the available width.

* It should be low boilerplate, both as a user and an implementor.

* You shouldn't need extra imports to configure the rendering for a type.

* If something is *almost* supported, you should be able to make it work.

Not all of these goals have been met. Maybe more will be in future.

## Solution

The approach I'm taking is to have a store of configuration options. Users can
write to it to specify how they want things to be rendered, and implementations
can read from it to decide how to render things, and if all goes well they'll
render things how the user wanted.

There are lots of ways users might want to customize rendering. The choices
someone might make when rendering a list (e.g. how many elements to show,
whether to try to count the remainder) are different from the choices they might
make when rendering a floating-point number (e.g. padding, number of decimal
places, whether to use `1e3` syntax). And the choices they might make when
rendering a custom data type that I've never heard of are different again.

So it won't do to have a fixed configuration type. We'll need a type family to
specify how each type can be configured.

Given this type family (let's call it `ConfigFor`), we could simply write
functions `ConfigFor a -> a -> String`. But then if you have a complicated data
structure, you need to specify the configuration options at every node. If you
have a `Map Int [Int]` and you want all ints to be printed with
underscore-separation, you might need something like

```haskell
defaultMapConfig
  { keysConfig = defaultIntConfig { separator = '_' }
  , valsConfig =
      defaultListConfig { elemsConfig = defaultIntConfig { separator = '_' } }
  }
```

or with something lensy it might be

```
defaultMapConfig
  & (#keysConfig . #separator .~ '_')
  & (#valsConfig . #elemsConfig . #separator .~ '_')
```

These are more verbose than I wanted, though honestly I could believe they're
better than what I went for. The other problem with them is that a `ConfigFor`
instance will often need to have a field for every field of every constructor of
a data type, plus some more; that seems like a lot.

Suppose we want something that lets us set config for all the `Int`s at once.
That sounds like some kind of map from types to `ConfigFor`s. Here's one way we
can implement that:

```haskell
class (Typeable a, Monoid (ConfigFor a), Typeable (ConfigFor a))
   => Configurable a
 where
  type ConfigFor a :: Type

data SomeConfigurable where
  SomeConfigurable :: Configurable a => !(TypeRep a) -> SomeConfigurable

newtype Config =
  UnsafeConfig { unsafeUnConfig :: Map SomeConfigurable Dyn.Dynamic }

configInsert :: Configurable a => ConfigFor a -> Config -> Config
configLookup :: Configurable a => Config -> ConfigFor a
instance Semigroup Config where { {- ... -} }
instance Monoid Config where { {- ... -} }
```

As long as we only interact with `Config` using `configInsert`, `configLookup`
and the `Monoid` and `Semigroup` instances, it works fine.

Now we can make a class for converting things to strings:

```haskell
class Configurable a => Gist a where
  gist :: Config -> a -> String
```

Except that for various reasons, it's more like

```haskell
class Configurable a => Gist a where
  gistPrec' :: Int -> Config -> a -> Doc ann
```

This is still a simplification, but the actual class just adds to it, it doesn't
change anything.

The reason that `Configurable` and `Gist` are two different classes, is that it
lets us attach configuration to non-data types; that is, types of kind other
than `Type`. For example, we can configure `[]` or `Maybe` or `Floating`. Then
`Gist` instances can look up configuration for those types, when it seems
relevant. If the instances for `Float` and `Double` both look at the
configuration for `Floating` as well as their own configuration, then users can
configure both simultaneously.

The reason for the `Int` is the same as in the standard `showsPrec`. It lets
instances decide whether or not to include parentheses depending on context. In
theory that information could be put into the `Config`, but currently it's not.

And
`[Doc](https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html#t:Doc)`
is from `prettyprinter`. It lets us handle the actual pretty-printing, in a way
that `String` simply can't.

## Usage

If you simply want to use `pretty-gist`, the intended set of imports is

```haskell
import qualified Gist
import Gist (gist)
```

If the default gist style for your data type is acceptable, then `gist [] val`
is sufficient. For example:

```haskell
gist [] (3 :: Int) -- -> "3"
gist [] ()         -- -> "()"
```

But if you want to customize rendering, you can use the functions `Gist.config`
and `Gist.strConfig`. For example:

```haskell
gist [Gist.strConfig @Float "%.2f"] (1.2345 :: Float) -- -> "1.23"
```

`Gist.config` requires you to know the configuration type for a given type. This
is handled by the type family `ConfigFor` in the class `Gist.Configurable`.
`Gist.strConfig` doesn't need that, but does need you to know how to specify its
configuration in string form. (If you make a mistake, that's a runtime error.)
Neither of these is ideal.

For example, `ConfigFor Float ~ Last (Maybe Printf.FieldFormat)`. `FieldFormat`
is the type that `Text.Printf` uses to control rendering; in future I expect to
replace it with something more expressive. (E.g. it has no way to render numbers
as `123_456` or `123,456`.) The `Maybe` is because we want some way to return to
the default rendering. And `Last` is because every `ConfigFor` instance has to
be a `Monoid` in case we try to configure the same thing twice. Currently every
instance is a product of `Last`s, and I'm not sure there's any reason for them
to be anything else.

`strConfig` kinda sucks, it's super not Haskelly. But it can be more succinct
than `Config`, and reduces the number of things you might need to import. I'm
inclined to recommend that you basically only use it temporarily while
debugging, plus for things like %-formatting which are already commonly
implemented through string parsing. But I guess we'll see how things land.

The first argument to `gist` is a list of all configurations you want to apply
to any type, and they get applied wherever the type appears in the value being
rendered.
