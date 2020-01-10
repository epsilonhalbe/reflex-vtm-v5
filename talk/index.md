# From Haskell to Android

## In a few easy steps

1. Learn Haskell ✓
1. Get an Android Phone ✓
1. Build Reflex-App ?
1. Joy! ✓

## In a few easy steps

1. Learn Haskell ✓
1. Get an Android Phone ✓
1. Build Reflex-App
     1. Install nix-pkg
     1. `nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command`
     1. `mkdir T-1000`
     1. `cd T-1000`
     1. `ob init`
     1. `nix-build -A android.frontend -o result-android`
     1. Enable debug mode on android
     1. `./result-android/bin/deploy`
1. Joy! ✓

## In a few easy steps

1. Learn Haskell ✓
1. Get an Android Phone ✓
1. Build Reflex-App
     1. Install nix-pkg
     1. `nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command`
        💤
     1. `mkdir T-1000`
     1. `cd T-1000`
     1. `ob init`
        💤
     1. `nix-build -A android.frontend -o result-android`
        💤
     1. [Enable debug mode on android][debug]
     1. `./result-android/bin/deploy`
1. Joy! ✓

# First learning of the day

## configure your caches properly

Your `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` should look like this

```{.nix}
substituters = … https://nixcache.reflex-frp.org …
trusted-public-keys = … ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= …
```
# Precaution

## Why you should use Reflex

- you want to learn FRP
- you want to do android/iOS/native/web/terminal application without the Java/Kotlin/C
  part and share a lot of the code-base
- want a production ready development tool to develop/run/deploy with nix
  and are willing to add the extra work hour to keep it that way, when it
  is not

## What you should keep in mind when learning Reflex

- People in the community are friendly and helpful and very intelligent
- there is no "established" architecture like the elm architecture, MVC etc.
- Documentation is bad => getting better
- sometimes you might end up with a blank screen - despite compiling without errors
- Obsidian Systems is a haskell consulting company developing it, not many others
- the learning curve is steep
- Don't ever think browsers work the same - e.g. implement a standard like WebRTC properly
  or at all

## Why you should *not* use Reflex

- GHCJS produces code that is humongous
- Sometimes you still need to touch Java/JavaScript and I guess Objective-C/Swift
- You know nothing about nix, and don't have the capacity to now learn that as well
- You need something mature & battle tested
- Simple haskell is your thing

# Some 

## Reflex - a type-class

```{.haskell}
class Reflex t where

  data Behavior t :: * -> *
  data Event t :: * -> *
  data Dynamic t :: * -> *

  … some more data …
  … some more methods …
```

## Reflex - a type-class

```{.haskell}
class ( MonadHold t (PushM t)
      , MonadSample t (PullM t)
      , MonadFix (PushM t)
      , Functor (Dynamic t)
      , Applicative (Dynamic t)
      , Monad (Dynamic t)
      ) => Reflex t where

  data Behavior t :: * -> *
  data Event t :: * -> *
  data Dynamic t :: * -> *
```

## Reflex - a type-class

```{.haskell}
class Reflex t where

  data Behavior t :: * -> *

  -- A container for a value that can change over time. Behaviors can be sampled at
  -- will, but it is not possible to be notified when they change

  data Event t :: * -> *
  data Dynamic t :: * -> *
```

## Reflex - a type-class

```{.haskell}
class Reflex t where

  data Behavior t :: * -> *
  data Event t :: * -> *

  -- A stream of occurrences. During any given frame, an Event is either occurring
  -- or not occurring; if it is occurring, it will contain a value of the given type
  -- (its "occurrence type")

  data Dynamic t :: * -> *
```

## Reflex - a type-class

```{.haskell}
class Reflex t where

  data Behavior t :: * -> *
  data Event t :: * -> *
  data Dynamic t :: * -> *

  -- A container for a value that can change over time and allows notifications on
  -- changes. Basically a combination of a Behavior and an Event, with a rule that
  -- the Behavior will change if and only if the Event fires.
```


## Handy Reflex functions

```{.haskell}
never    :: Event t a
constant :: a -> Behavior t a
switch   :: Behavior t (Event t a) -> Event t a
current  :: Dynamic t a -> Behavior t a
updated  :: Dynamic t a -> Event t a
hold     :: a -> Event t a -> m (Behavior t a)
holdDyn  :: a -> Event t a -> m (Dynamic t a)
foldDyn  :: (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
leftmost :: [Event t a] -> Event t a
tag      :: Behavior t b -> Event t a -> Event t b
zipDyn   :: Dynamic t a -> Dynamic t b -> Dynamic t (a, b)
gate     :: Behavior t Bool -> Event t a -> Event t a
```

# Implementing a simple text input widget

## CODE

```{.haskell}
inputWidget
  :: forall t m
  .  DomBuilder t m
  => Text
  -> m (Dynamic t Text)
inputWidget name
  = fmap value
  $ elClass "div" "mui-textfield"
  $ inputElement
  $ def & inputElementConfig_elementConfig
        . elementConfig_initialAttributes .~ [("placeholder", name)]
```

## Using it

Might just lead to a slight knot in our brain

## Prototype I

```{.haskell}
app :: … => m()
app = elClass "div" "app" do
  el "h1" $ text "Prototype 1"
  void $ inputWidget "Test"
```

## Prototype II

```{.haskell}
app :: … => m()
app = elClass "div" "app" do
  el "h1" $ text "Prototype 2"
  txt <- inputWidget "Test"
  el "br" blank
  dynText (("Test: " <>) <$> txt)
```

## Final Widget

```{.haskell}
app :: … => m()
app = elClass "div" "app" do
  el "h1" $ text "Input Widget"
  dynText (("Test: " <>) <$> txt)
  el "br" blank
  txt <- inputWidget "Test"
  blank
```

## Final Widget

```{.haskell}
{-# LANGUAGE RecursiveDo #-}

app :: … => m()
app = elClass "div" "app" mdo
  el "h1" $ text "Input Widget"
  dynText (("Test: " <>) <$> txt)
  el "br" blank
  txt <- inputWidget "Test"
  blank
```

## Final Widget

```{.haskell}
app
  :: forall t m
  .  ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     )
  => m ()
app = elClass "div" "app" mdo
  el "h1" $ text "Input Widget"
  dynText (("Test: " <>) <$> txt)
  el "br" blank
  txt <- inputWidget "Test"
  blank
```

## Caveats

Reflex relies HEAVILY on lazyness, in fact `MonadFix`,
is implemented in terms of `unsafeInterleaveIO`.

The following things

- `{-# LANGUAGE StrictData #-}`
- `{-# LANGUAGE BangPatterns #-}`
- `case`-statements
- `let`-statement
- (anonymous-)function with type matching

might interfere with your code and `~` becomes your best friend.

# A Dropdown Widget

## Note

There is a dropdown input widget available but that compiles to a `select-input`
and the MUI-example uses just divs and looks so much better

https://www.muicss.com/docs/v1/css-js/dropdowns

```{.html}
<div class="mui-dropdown">
  <button class="mui-btn mui-btn--primary" data-mui-toggle="dropdown">
    Dropdown
    <span class="mui-caret"></span>
  </button>
  <ul class="mui-dropdown__menu">
    <li><a href="#">Option 1</a></li>
    <li><a href="#">Option 2</a></li>
    <li><a href="#">Option 3</a></li>
    <li><a href="#">Option 4</a></li>
  </ul>
</div>
```

## Start simple

```{.haskell}
dropdownButton :: … => Dynamic t (Maybe a) -> m (Event t ())
dropdownButton mCurrentElement = do
  (e,_) <- elCass' "button" "mui-btn mui-btn--large mui-btn--primary block"
            $ dynText $ maybe "(select)" (pack . show) <$> mCurrentElement
  pure $ domEvent Click e
```

```{.haskell}
li :: … => a -> m (Event t a)
li x =
  el "li" do
    (e, _) <- elClass' "a" "dropdown-link" $ text . pack $ show x
    pure $ domEvent Click e $> x
```

With routing the whole `href=#` from MUI thingy breaks 🙁 => `dropdown-link` as CCS, since
we don't want to use additional JavaScript to hook into our App, because `ob run` does SSR,
which does not like willy nilly DOM manipulation.

## Get more complex

```{.haskell}
selectItems :: … => Proxy a -> m (Event t (Maybe a))
selectItems _
  = fmap Just . leftmost <$> traverse li ([minBound .. maxBound] :: [a])
                ^^^^^^^^
```

## … and more

```{.haskell}
clicked :: … => Event t b -> m (Dynamic t Text)
clicked click
  =  fmap (("mui-dropdown__menu fullwidth" <>) . bool "" " mui--is-open")
 <$> foldDyn (const not) False click
     ^^^^^^^
```





[reflex]: https://reflex-frp.org/
[debug]: https://www.embarcadero.com/starthere/xe5/mobdevsetup/android/en/enabling_usb_debugging_on_an_android_device.html
