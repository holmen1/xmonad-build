# xmonad-build


### Add dependencies

```
$ yay -S \
> xorg-server xorg-apps xorg-xinit xorg-xmessage \
> libx11 libxft libxinerama libxrandr libxss \
> pkgconf
```
```
$ mkdir -p ~/.config/xmonad && cd ~/.config/xmonad
```

Create a file called xmonad.hs with the following content:
```haskell
import XMonad

myModMask       = mod4Mask -- Rebind Mod to the Super key
myTerminal      = "kitty"


main :: IO ()
main = xmonad $ def
    { modMask    = myModMask,
      terminal   = myTerminal
    }
```

Still in ~/.config/xmonad, clone xmonad and xmonad-contrib repositories using git:
```
$ git clone https://github.com/xmonad/xmonad
$ git clone https://github.com/xmonad/xmonad-contrib
```

Install Haskell via [ghcup](https://www.haskell.org/ghcup/#)

For the simple, interactive, text-based user interface (TUI), run:
```
ghcup tui
```

You’ll need to update the cabal package index, build xmonad and xmonad-contrib libraries and then build the xmonad binary:
```
$ cabal update
$ cabal install --package-env=$HOME/.config/xmonad --lib base xmonad xmonad-contrib
$ cabal install --package-env=$HOME/.config/xmonad xmonad
```

### Configure .xinitrc

To start `xmonad` using `startx`, create a `.xinitrc` file in your home directory with the following content:

```bash
#!/bin/bash
exec xmonad
```

Make the file executable:

```bash
chmod +x ~/.xinitrc
```

Now you can run `startx` to launch `xmonad`.