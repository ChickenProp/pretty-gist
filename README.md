(This README is incomplete.)

This is an exploration of a new-to-me approach to stringification.

The lowest-friction way to stringify things in Haskell is usually `show`. It
gives the user close to zero ability to control how the thing is rendered.

The
[`Show1`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Classes.html#t:Show1)
and
[`Show2`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Classes.html#t:Show2)
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
  numbers, and I want them rounded to 3dp with underscore separation
  (`17_923.472`). Or strings, which I want printed using C-style escapes rather
  than Haskell-style, and with unicode rendered.

* A list might be infinite, and I only want to show the first ten elements. Or a
  tree might actually be cyclic, and I want to only show three levels deep.

`pretty-gist` aims to enable stuff like this. I call the rendered outputs it
produces "gists" following Raku's use of that term, where I think the intention
is something like "take a guess about what I'm likely to want to see here and
show me that". But if `pretty-gist` guesses wrong, it lets you correct it.

I've come up with several different approaches to this, which all have different
things to recommend and disrecommend them. I'm writing about three of them here.

## Design goals

* As a user, you should be able to target configurations specifically or
  generally. "All lists" or "all lists of Ints" or "only lists found at this
  specific point in the data structure". "All floating-point numbers" or
  "`Float` but not `Double`".

* It should pretty-print, with indentation that adjusts to the available width.

* It should be low boilerplate, both as a user and an implementer.

* You shouldn't need extra imports to configure the rendering for a type.

* If something *almost* works, you should be able to make it work. No need to
  throw everything out and start from scratch.

I don't know how to meet all these design goals at once, but they're things I
aim for.

## Classless solution

Perhaps the very simplest solution is just to write a custom renderer every time
I need one. I'm not going to do that.

A level up from that is to write renderers for lots of different data types. We
can write

```haskell
newtype Prec = Prec Int -- precedence level, 0 - 11

data ConfigMaybe = ConfigMaybe { showConstructors :: Bool }
gistMaybe :: ConfigMaybe -> (Prec -> a -> Doc) -> Prec -> Maybe a -> Doc

data ConfigList = ConfigList { showElems :: Maybe Int, ... }
gistList :: ConfigList -> (Prec -> a -> Doc) -> [a] -> Doc

data ConfigTuple2 = ConfigTuple2 { ... }
gistTuple2
  :: ConfigTuple2 -> (Prec -> a -> Doc) -> (Prec -> b -> Doc) -> (a, b) -> Doc

data ConfigFloat = ConfigFloat { ... }
gistFloat :: ConfigFloat -> Float -> Doc
```

for some data type `Doc` that supports layout. (I've been using the one from
`prettprinter`, which has a type argument that I'm ignoring here for
simplicity.)

The `Prec` parameters here are needed for the same reason `Show` has
`showsPrec`. Sometimes we need parentheses, and this lets us choose when we have
them. But they clutter things up a lot. We could imagine predence being
something that gets put into config types, but then the user needs to specify it
everywhere; plus, a renderer might call one of its sub-renderers at two
different precedence levels under different circumstances. So that doesn't
really work, so we accept the clutter.

But essentially, we've decided that one particular config parameter is important
enough that every instance takes it (which means every instance *knows* that
every instance takes it). That feels kinda dirty. Is there anything else that
ought to be given this treatment?

Anyway, this works, and it has some things to recommend it. It's incredibly
simple, a beginner-level Haskell programmer will be able to figure out what's
going on. If I, as the library author, make a decision you the library user
don't like, you can just write your own function and it plugs in seamlessly. And
if you have a type that can't normally be rendered, like a function, you can
pick some way to render it anyway.

It also has some things to disrecommend it. Most notably, it's very verbose. You
need to specify how to render every type-parameterized node of your data
structure. You can have default configs, but there's no "default list renderer"
because there's no "default list element renderer". `IntMap v` can have a
default renderer for its keys, but `Map Int v` can't.

This also means that changes to your data structure are very often going to need
to be reflected in your renderers, which sounds tedious.

Another problem is, consistency is going to be hard. In the example above, I had
`gistMaybe` take a precedence parameter, but not the others. That's because the
others would ignore it. So either I include a useless parameter and you have to
think about what value to pass (because you might not realize it's useless); or
I make you think about whether that parameter is there or not. I did have them
pass a precedence to their element renderers, but they'll always pass `0`, so
maybe I shouldn't bother? Whatever decision I make, someone implementing their
own renderers is going to choose differently.

## One-class solution

We can maybe-improve on this with typeclasses. We can use type families to let
each gistable type have a separate config type.

```haskell
class Gist a where
  type Config a :: Type
  defaultConfig :: Config a
  gistPrec :: Prec -> Config a -> a -> Doc

data ConfigList a = ConfigList
  { showFirst :: Maybe Int
  , configElem :: Config a
  }

instance Gist a => Gist [a] where
  type Config [a] = ConfigList a
  defaultConfig = ConfigList { showFirst = Nothing, configElem = defaultConfig }
  gistPrec = ...
```

This is the foundation of what I formerly called the "simple" approach, naming
it in comparison to things I haven't shown you yet, which is implemented in the
module `Gist.Simple`.

There are a few significant complications. One is, this won't handle `String`
well, because that's just `[Char]`. Other typeclasses (including `Show`) solve
this by having an extra method for "how to handle lists of this type"; then you
give that method a default implementation, but override it for `Char`. This
seems fine as a solution, by which I mean "I hate it but I don't have any better
ideas". I'm not going to bother showing it here. (Also it's not actually
implemented for this solution in the code yet.)

Next: the typechecking here doesn't work very well. If we try

```haskell
gist (defaultConfig { ... }) [True] -- `gist` is `gistPrec 0`
```

we probably mean `defaultConfig` to refer to `defaultConfig @[Bool]`. But all
GHC knows is that we mean `defaultConfig { ... }` to have type `Config [Bool]`.
That doesn't even fully specify the type of `defaultConfig`, let alone its
value. (We might be using the `defaultConfig` of some other instance that shares
the same type; or that has a different type until we update it. The second
possibility means injective type families wouldn't help much.) So we instead
need to write

```haskell
gist ((defaultConfig @[Bool]) { ... }) [True]
```

which is no fun. That extra unnecessary-looking set of parentheses is an added
kick in the teeth. So we add functions `gistF` and `gistPrecF`, which replace
the `Config a` argument with a `Config a -> Config a` argument and apply it to
`defaultConfig`. So now we write

```haskell
gistF (\c -> c { ... }) [True]
```

But we do keep the existing functions around for the final complication. Some
things can't be rendered automatically (e.g. functions), but we sometimes want
to render them anyway, or data structures containing them. Like a function
`Bool -> Int` that's equivalent to a pair `(Int, Int)`. Sometimes we can use
newtypes and maybe `coerce` for this, but not always, and it might be a pain
even if we can.

It turns out we can handle this case. Consider the type

```haskell
data Gister a where
  FnGister :: (forall ann . Int -> a -> Doc ann) -> Gister a
  ConfGister :: Gist a => Config a -> Gister a

runGister :: Int -> Gister a -> a -> Doc ann
runGister prec = \case
  FnGister   f -> f prec
  ConfGister c -> gistPrec prec c
```

A `Gister` is a renderer for any type. For types implementing `Gist`, we can
create a `Gister` through `Config`, but for any other type we can still write
our own rendering function.

This lets us have an instance `Gist [a]` without first requiring `Gist a`. We
can't have a (useful) default config in that case, the only fully general
`Gister a`s we could write would ignore the value they're passed. But we can
resolve that:

```haskell
class Gist a where
  type Config a :: Type
  type HasDefaultConfig a :: Constraint
  defaultConfig :: HasDefaultConfig a => Config a
  gistPrec :: Int -> Config a -> a -> Doc ann

data ConfigList a = ConfigList
  { showFirst :: Maybe Int
  , gistElem  :: Gister a
  }

instance Gist [a] where
  type Config [a] = ConfigList a
  type HasDefaultConfig [a] = (Gist a, HasDefaultConfig a)
  defaultConfig =
    ConfigList { showFirst = Nothing, gistElem = defaultConfig }
  gistPrec = ...
```

So now you can call `gist` on a `[Bool -> Int]`, and you need to write for
yourself how to render one of those functions but you can use `gist` when you do
so. There's no `defaultConfig @[Bool -> Int]`, but you can do a type-changing
update of `defaultConfig @[Void]` or similar. Though this is harder than we
might like, because we can't derive a `Generic` instance for `Gister a` which
means we can't use generic-lens or generic-optics. Fine in this case, annoying
for nested structures, might be able to improve.

And that's just about everything.

So why wasn't I happy with this solution? Why did I go looking for others? There
are two or three specific problems I was trying to solve.

There's a problem for implementers. As an implementer, you're responsible for
capturing and passing on configuration for, roughly speaking, every field of
every constructor of your type. If e.g. the `IntMap` instance forgets that
people might want to configure how it renders the keys, then the keys won't be
configurable.

I think the other approaches I have kinda mitigate this problem, but I wouldn't
say "solve". The `Gister` thing also mitigates it, since users can essentially
write their own instances, but I didn't come up with it until after I'd gone
exploring. Also there might be clever things people can do with various forms of
generic (not necessarily `Generic`) traversal.

There's also a problem for users. As a user, I expect that you'll usually want
to configure every value of a type in the same way, at any given call to `gist`.
If you have a `[(Float, Float)]`, you probably want to render the `fst`s the
same as the `snd`s.

But to do that, you just have to find every place in your data structure that
has or might have a Float, and configure them all. I think the other approaches
I have mostly solve this problem, but with added complexity when you don't want
to do that.

And, honestly, I wasn't enthusiastic about doing lots of record updates. There's
a bunch of [things I don't
know](https://www.reddit.com/r/haskell/comments/128aifn/monthly_hask_anything_april_2023/jepke16/)
offhand about how to work with records, and I think some changes planned in
upcoming GHC releases, but my impression was it would be kind of a pain. The
other approaches let users do configuration through strings as well as record
updates, but strings also kinda suck for this. But even if you stick to records,
they let you avoid nested updates, which is nice.

Anyway, let's look at what else I came up with.

## Dynamic solution

If we don't want users to have to specify the config for a type at every point
the type might be used, one thing we might try is to have a store of
configuration options. Users can write to it to specify how they want things to
be rendered, and implementations can read from it to decide how to render
things, and if all goes well they'll render them how the user wanted.

But we still want different config options for different types. So we have a
type family like in the simple solution, except here I've called it `ConfigFor`
to free up the name `Config`.

What we want here is essentially a map from types to `ConfigFor`s. Again
ignoring the problem with `String`, here's one way we can implement that:

```haskell
class (Typeable a, Monoid (ConfigFor a), Typeable (ConfigFor a))
   => Configurable a
 where
  type ConfigFor a :: Type

data SomeConfigurable where
  SomeConfigurable :: Configurable a => !(TypeRep a) -> SomeConfigurable

newtype Config =
  UnsafeConfig { unsafeUnConfig :: Map SomeConfigurable Dynamic }

config :: Configurable a => ConfigFor a -> Config
configLookup :: Configurable a => Config -> ConfigFor a
instance Semigroup Config where ...
instance Monoid Config where ...
```

Those last four lines declare an interface to `Config` that lets us use it
safely. We use `mempty` to create an empty `Config`, `config` to create a
singleton, `(<>)` to combine two `Config`s, and `configLookup` to read from one.

We then define gisting functions in terms of `Config`,

```haskell
class Configurable a => Gist a where
  gistPrec' :: Int -> Config -> a -> Doc ann

gistPrec :: Gist a => Int -> [Config] -> a -> Doc ann
gistPrec prec confs = gistPrec' prec (mconcat confs)

gist :: Gist a => [Config] -> a -> Doc ann
gist = gistPrec 0
```

where `gist` and `gistPrec` are slightly more convenient to call than the actual
class method.

What does an instance for this look like?

```haskell
data ConfigList a = ConfigList
  { showFirst :: Last (Maybe Int)
  , configElem :: Last Config
  }
instance Semigroup (ConfigList a) where ...
instance Monoid (ConfigList a) where ...

instance Configurable [] where
  type ConfigFor [] = ConfigList

instance Typeable a => Configurable [a] where
  type ConfigFor [] = ConfigFor [a]

instance Gist a => Gist [a] where
  gistPrec' _ conf l =
    let ConfigList { showFirst, configElem } =
          configLookup @[] conf <> configLookup [a] conf
        showFirst_ = fromMaybe Nothing $ getLast showFirst
        configElem_ = fromMaybe conf $ getLast configElem
    in  ...
```

We see here the reason that `Configurable` and `Gist` are now distinct classes.
The user can configure all lists, and they can override that configuration for
lists of a specific type. Similarly, we can write `instance Configurable
Floating`, and the `Gist` instances for both `Float` and `Double` read that
config value, to allow users to configure both those types at once.

We might use this interface like so:

```haskell
result = gist [config @[] $ mempty { showFirst = pure (Just 5) }] [True]
```

If you don't want all instances of a particular type to be configured the same,
that's still possible (assuming the instance was implemented to allow it). You
have to use stuff like the `configElem` field:

```haskell
result = gist
  [ config @Bool $ mempty { ... }
  , config @[] $ mempty { configElem = pure $ config @Bool $ mempty { ... }
  ]
  (True, [False])
```

I previously mentioned not liking record updates. So we can allow string
configuration by adding a parsing method to `Configurable`,

```haskell
class (Typeable a, Monoid (ConfigFor a), Typeable (ConfigFor a))
   => Configurable a
 where
  type ConfigFor a :: Type
  parseConfigFor :: String -> Either String (ConfigFor a)

strConfig :: forall a . (HasCallStack, Configurable a) => String -> Config
strConfig s = case parseConfigFor @a s of
  Left  err     -> error $ "Could not parse config: " <> err
  Right confFor -> config @a confFor

result = gist [strConfig @[] "show-first 5"] [True]
```

The nice thing about this is that you don't need to worry about whether the
record field is imported or where it needs to be imported from. But it's bad in
so many ways that I'll be surprised if I keep it around.

How does this compare to the simple solution?

I do expect it's simpler in a lot of common use cases. That's valuable. And it's
probably more likely to keep working, and doing what you want, if you change the
datatype that you're calling it on.

But there are some complicated cases where it'll be more awkward to get it to do
what you want. If you do want to configure some instances of a type differently
from others, you need to fiddle around with things like the `configElem` field
of `ConfigList`.

As an implementer, it's on you to remember to make such fields available.
Roughly speaking you *still* need a config field for every field of every
constructor of your type. And I suspect you're more likely to forget, and
they're more boilerplate to deal with. On the plus side it's probably going to
be less of an inconvenience for users if you do forget.

Implementers also need to make a decision for these fields: do the `Config`
values they contain *replace* the top-level `Config` value or *augment* it? That
is, suppose I use the configuration

```haskell
[ config @Foo $ mempty { configA = pure 1 }
, config @[] $ mempty { configElem = config @Foo $ mempty { configB = pure 1 } }
]
```

When rendering a `[Foo]`, we might expect its configuration to be either of

```haskell
mempty { configB = pure 1 } -- or
mempty { configA = pure 1, configB = pure 1 }
```

Which do we choose? That's up to each implementer, and as I write this, I'm not
sure which I chose myself for the instances I wrote. Maybe I wasn't even
consistent. I certainly don't expect everyone else to be, which makes a bad
experience for users.

And suppose that, as a user, you don't understand how your complicated set of
config options is getting interpreted by instances and turned into the set of
config values they actually use. What types does an instance call `configLookup`
for? In what order? You'd better hope instances are well-documented and/or
(let's face it, and) have source code available.

Also, newtype deriving doesn't get good results. It's not entirely clear to me
what good results would look like - we might expect the `Gist` instance for
`newtype MyFloat = MyFloat Float` to pay attention to config values for
`MyFloat` as well as for `Float`, or perhaps it should be "instead of". But what
actually happens is it pays attention to config values for `Float` and not
`MyFloat`, which is certainly wrong. There isn't even a super convenient way to
piggyback off the existing instance for `Float` if you define the `MyFloat`
instance manually.

So there's one more approach I have, with a different set of tradeoffs.

## Monadic solution

The idea behind this is similar to the dynamic solution. But we

1. Formalize some of the things that pretty much every instance would have done
   anyway, and make them official parts of the interface.

2. Track the path of types we've walked to get here, and let users match on that
   when choosing when their config options apply.

3. Use a Monad to keep track of some state, and potentially give the user some
   debugging aid.

Probably some of the changes could be picked and chosen, giving various
different designs in between the Dynamic and Monadic solutions, but I'm not
going to evaluate them all individually.


(TBC. Everything below is basically editing notes that I may incorporate
properly later. Not really intended for public consumption but no reason not to
publish them I guess. Much of the above is out of date too.)

---







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

## Records

In general it would be nice to use records to configure things. But Haskell's
record story is not great.

For lots of data structures we might want the option to only show the beginning.
In current implementations I've called it "show first", but "truncate at" might
be another sensible choice. We could imagine applying this to list, maps, sets,
strings, etc.. And then we could also have a "count remaining" option so the
user can see how many elements were omitted.

So do the records we use to configure this type, all have fields `truncateAt ::
Maybe Int, countRemaining :: Bool`? Certainly we'd like users to be able to
guess what these options are called, supposing an instance has them.

I don't think duplicate record fields work very well right now. (It's possible
they've improved recently.) But I'm also not enthusiastic about prefixing every
field in every config type.

A second problem with records is importing.

It may be that these problems are solved by anonymous records. But that seems
like a very large hammer, and increases dependency footprints.

Using generic-lens and/or generic-optics might also be fine as a solution. Users
who don't want to rely on those could still do the awkward workarounds.

## Comparison

### Simplicity

Unsurprisingly, the Simple interface is easiest to understand, which seems
valuable. It works by construction, with invalid types being unrepresentable.
The other interfaces are less transparent, and use types that could hold invalid
values if I wrote a bug. Additionally, the monadic interface relies on
higher-kinded data.

### Ease of use

To a large extent this is an open question, and I don't think there's going to
be an easy answer to it.

### Ease of implementation

That is, how easy is it to write an instance?

### Overridability

That is, if an instance doesn't exist, or doesn't do quite what you want, can
you as a user do something about it?

The Simple interface wins here: if you don't like the `Gist` instance for a
type, you can simply write your own renderer and use that.

```haskell
gist (defaultConfig & #gistElem .~ FnGister (\() -> "_")) [()] -- "[_]"
```

This even works if the type doesn't have its own instance. A caveat is there's
then no `defaultConfig`, but you can just use a different `defaultConfig` and
change the type afterwards:

```haskell
gist (defaultConfig @[Void]
      & #gistElem
      .~ FnGister (\f -> gist defaultConfig $ f ()))
     [\() -> 3] -- "[3]"
```

(TODO: do these work? The first one might also need its type specified.)

I think I'd be able to make the "write your own renderer" thing work for the
other interfaces. At any rate, any individual instance can have as an option
"use some custom renderer instead of the one specified here". So I could hard
code that as a config option to every instance, and if I play my cards right it
might not even inconvenience implementers.

But you'd only be able to override existing instances like that. You wouldn't be
able to ignore the lack of an instance, and I don't see a way to make that
possible. In the simple interface, it's possible to create a value of type
`Config [() -> Int]` even though there's no `instance Gist (() -> Int)`; and
that value is enough to gist a value of type `[() -> Int]`.

But in the other interfaces, to gist a value of type `[a]`, the configuration
type is simply `Config`. We may know that the `Config` includes instructions for
rendering `a` that have nothing to do with the `Gist a` instance; but the
compiler doesn't know that. Maybe it's possible if you somehow attach a
type-level list to the `Config` saying which instances are unnecessary, but that
sounds unergonomic.

## Open questions

### How well do records work for this?

It seems natural to use records for config types, but Haskell's records are
kinda crufty. (And so is Haskell's import syntax, and so is the interaction
between the two.)

I'd prefer to be able to do updates without either explicitly importing a record
field or qualifying it. That is, I don't like either of

```haskell
import Some.Data.Type (GistConfig(..))
result = gist (defaultConfig { someOption = 3 }) val

import qualified Some.Data.Type as Type
result = gist (defaultConfig { Type.someOption = 3 }) val
```

Both have the problem that you need to know where the `Gist` instance for a type
is defined. If it's defined next to the type itself, you'll typically find its
configuration type in the same module. (But maybe not, if it borrows another
type's config and doesn't re-export it.) If it's defined next to the `Gist`
class, you'll find its configuration type in the `Gist` module.

It's probably not that big a deal. Still, I'd prefer users not to have to worry
about this. [Anonymous
records](https://hackage.haskell.org/package/large-anon-0.2/docs/Data-Record-Anon-Simple.html)
might be one solution, downside is that package has a lot of dependencies but
upside is they might be useful for other reasons, see next section. I haven't
investigated them closely. [Generic lenses]() (or [generic optics]()) might be
another, which would only affect

Possible awkward: consider something like

```haskell
data Pair a = Pair { p1 :: a, p2 :: a }
data ConfigPair a = ConfigPair { gistP1 :: Gister a, gistP2 :: Gister a }
instance Gist (Pair a) where { type Config (Pair a) = ConfigPair a; ... }

val :: Pair (() -> Int)
```

Suppose we want to gist `val`. As a record upate, and using lenses, the configs
look like:

```haskell
defaultConfig @(Pair Void) { gistP1 = FnGister (...), gistP2 = FnGister (...) }

defaultConfig @(Pair Void)
  & #gistP1 .~ FnGister (...)
  & #gistP2 .~ FnGister (...)
```

But the second doesn't type check, because after the first update `gistP1` and
`gistP2` have different types. From a quick glance at the docs I'm not even sure
large-anon supports type-changing record updates; if it does it may or may not
have the same problem.

Possible solution: we actually have

```haskell
data ConfigPair a b = ConfigPair { gistP1 :: Gister a, gistP2 :: Gister b }
instance Gist (Pair a) where { type Config (Pair a) = ConfigPair a a; ... }
```

Some of these problems partially go away if we allow using strings for config.
But not all the way, and my guess is the cure is worse than the disease.

### Can we make `DerivingVia` or `DeriveAnyClass` work?

A sensible default for a product type is likely "render similar to how the
`Show` instance would, but have a config option for each of your fields". Can we
make that work through a `Generic` instance?

This probably needs anonymous records. I can't think how else to do it.

Better yet would be if we can have a custom instance write a record, and say
"include in this record the config options that would have been generated
generically". (Ideally as a subrecord, not a record-valued field.)

If we can't make these work, I suppose template Haskell is an option. I'm not
sure how annoying it would be. Also, it could conceivably be possible to derive
the implementation for the `gistPrec` function generically, if the user defines
the `Config` type in a specific way.

### Does the PVP cause us problems?


### What is this library trying to be?

Do I expect users to be able to use it to get the renders they want, down to the
character (and adjusting correctly for width)? That feels too ambitious. But
then how much control do I want them to have? Should they be able to choose
between various different layout options and indent widths? Honestly, probably
*also* too ambitious.

Some places this library might get used:

1. Debugging output that never gets committed
2. Test output that sticks around but is never seen by users
3. Debug output that might get seen by users, but only when things go wrong
4. Ouptut that's fully intended and expected to be seen by users in the normal
   course of events

If we can do one we can do any of the previous ones, but focusing on an earlier
number might make it easier to use for that purpose?

### What would be helpful

I don't actually have a lot of case studies in my head. Most of the places I've
wanted something like this have been at work, and I can't share that code and
it's too complicated to be a good case study. Haskenthetical is more promising.
But if readers can think of times they would have found something like this
useful, I'd love to know about it, especially if there are details that might
influence the design.






The Simple interface is easiest to understand, which seems valuable. It supports
containers of non-gistable things. (You can gist e.g. `[(+ 1), (* 2)]`, though
there's no default config for that.) It also gives you full control over
rendering of gistable things, if the existing instance doesn't do what you want.

One downside is verbosity in use. There's no way to say "yes, every Float should
be rendered like...", you just need to point at every Float in your data
structure. How big a deal is this? Depends on common use cases. If it turns out
to be a problem, it may be possible to take ideas from one of the other
implementations, and use them to generate a config for this implementation.

Another is that it's easier to write a badly-behaved instance. If you simply
don't include subconfigs for some part of your data, users won't be able to
configure it. How big a deal is this? Partly depends what quality of generic
implementation I can reach.

There's no configuration by string. I want users to be able to make changes
without fiddling with imports. I think that something like

```haskell
import Gist (gist)
result = gist (defaultConfig { showFirst = Just 2 }) [...]
```

doesn't work, and perhaps can't be made to, because of scoping; though it may be
that `HasField` related stuff will change that in future? Even if we don't mind
the imports, duplicate record fields might make things very awkward. Meanwhile,
I'm confident that

```haskell
import Gist (gist)
result = gist (defaultConfig & #showFirst .~ Just 2) [...]
```

can be made to work, but maybe only if the user relies on generic-lens or
generic-optics.
