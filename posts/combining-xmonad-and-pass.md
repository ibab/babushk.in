---
title: Combining XMonad with pass
date: 2014-02-15
---

Recently, I've resolved to stop reusing the same password over and over again on every website that I'm registered on.
Of course, remembering a new password for every website is extremely difficult, especially if you generate them randomly.

This is where password managers come in.
Typically, these applications manage a password database that is secured with a master password.
[pass](http://www.zx2c4.com/projects/password-store/) is one such program that I'm particularly fond of.
It's a very minimal command line utility that uses `gpg` to encrypt your passwords and (optionally) `git` for tracking changes and creating backups.
You can generate and save a password with

    $ pass generate [name] [length]

and copy a password to the clipboard with

    $ pass -c [name]

It's great when working on the command line, but is a bit inconvenient when you're entering passwords on the web, because you have to switch to a terminal to use it.

[XMonad](http://xmonad.org/) is my favourite tiling window manager.
It is configured in [Haskell](http://www.haskell.org/haskellwiki/Haskell) and is extremely convenient for programming and working in the terminal.
It occured to me that XMonad can be easily extended to interface with applications like pass.
The xmonad-contrib package includes scaffolding for creating custom prompts that query the user for input and offer tab completion.

Include the following Modules in your `~/.xmonad/xmonad.hs`:

```haskell
import System.Environment
import System.FilePath.Posix
import XMonad.Prompt
```

A new barebones prompt can be configured in a few lines of code:

```haskell
data Pass = Pass

instance XPrompt Pass where
  showXPrompt       Pass = "Pass: "
  commandToComplete _ c  = c
  nextCompletion      _  = getNextCompletion

passPrompt :: XPConfig -> X ()
passPrompt c = do
  li <- io getPasswords
  mkXPrompt Pass c (mkComplFunFromList li) selectPassword
```

Now, we need functions for interfacing with pass:

```haskell
selectPassword :: String -> X ()
selectPassword s = spawn $ "pass -c " ++ s

getPasswords :: IO [String]
getPasswords = do
  user <- getEnv "USER"
  entries <- getDirectoryContents $
    "/home/" ++ user ++ "/.password-store"
  return $ map takeBaseName entries
```

Bind a key to `passPrompt promptConfig` for opening the prompt.

Finally, the prompt can be styled to your liking:

```haskell
promptConfig = defaultXPConfig
  { font        = "xft:Source Code Pro:pixelsize=12"
  , borderColor = "#1e2320"
  , fgColor     = "#dddddd"
  , fgHLight    = "#ffffff"
  , bgColor     = "#1e2320"
  , bgHLight    = "#5f5f5f"
  , height      = 18
  , position    = Top
  }
```

The `Pass:` prompt autocompletes the names in the password database and makes working with pass extremely convenient.

![](/files/password-prompt.png)

Please note that in the code snippet above, only top-level passords in `~/.password-store` are included in the list of options.
This could be extended by recursing into subdirectories in `getPasswords`.

This example could easily be used to implement other custom prompts.
Give it a try!

You can find my complete XMonad configuration on [Github](https://github.com/ibab/dotfiles/blob/master/xmonad/.xmonad/xmonad.hs).


