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



---
To create a cleaner, factory-like build process for `xmonad` without relying on `pacman` or cluttering `~/.config/xmonad`, you can set up a dedicated build directory and use `cabal` to manage dependencies and builds. Here's a step-by-step guide:

---

### 1. **Set Up a Clean Build Directory**
Create a dedicated directory for building `xmonad` and its dependencies:

```bash
mkdir -p ~/xmonad-build && cd ~/xmonad-build
```

---

### 2. **Clone the Repositories**
Clone the `xmonad` and `xmonad-contrib` repositories into this directory:

```bash
git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib
```

---

### 3. **Initialize a Cabal Project**
Create a `cabal.project` file in the `~/xmonad-build` directory to manage the build environment:

```bash
cat > cabal.project <<EOF
packages: ./xmonad ./xmonad-contrib
EOF
```

This tells `cabal` to treat the cloned repositories as local packages.

---

### 4. **Build xmonad**
Run the following commands to build `xmonad` and its dependencies:

```bash
cabal update
cabal build xmonad
```

This will build `xmonad` in an isolated environment without polluting your global Haskell setup.

---

### 5. **Install the xmonad Binary**
Once the build is complete, install the `xmonad` binary to a location in your `PATH`. For example:

```bash
cabal install xmonad --installdir=$HOME/.local/bin --overwrite-policy=always
```

Ensure `$HOME/.local/bin` is in your `PATH`:

```bash
export PATH=$HOME/.local/bin:$PATH
```

You can add this line to your shell configuration file (e.g., `~/.bashrc` or `~/.zshrc`) for persistence.

---

### 6. **Configure xmonad.hs**
Instead of cloning repositories into `~/.config/xmonad`, you can create a minimal `xmonad.hs` file in `~/.config/xmonad`:

```bash
mkdir -p ~/.config/xmonad
cat > ~/.config/xmonad/xmonad.hs <<EOF
import XMonad

myModMask       = mod4Mask -- Rebind Mod to the Super key
myTerminal      = "kitty"

main :: IO ()
main = xmonad $ def
    { modMask    = myModMask,
      terminal   = myTerminal
    }
EOF
```

---

### 7. **Rebuild xmonad After Configuration Changes**
Whenever you update `xmonad.hs`, recompile it with:

```bash
xmonad --recompile
```

---

### 8. **Optional: Use a Script for Automation**
You can create a script to automate the build process:

```bash
cat > ~/xmonad-build/build-xmonad.sh <<EOF
#!/bin/bash
set -e

cd ~/xmonad-build
cabal update
cabal build xmonad
cabal install xmonad --installdir=\$HOME/.local/bin --overwrite-policy=always
EOF

chmod +x ~/xmonad-build/build-xmonad.sh
```

Run the script whenever you want to rebuild `xmonad`.

---

### Benefits of This Approach:
1. **Clean Directory Structure**: Keeps the build process isolated in `~/xmonad-build`.
2. **No Redundant Package Manager**: Avoids using `pacman` or `yay` for Haskell-related dependencies.
3. **Customizable Builds**: You can easily modify or update `xmonad` and `xmonad-contrib` without affecting your global environment.
4. **Portable**: The setup can be reused on other systems by copying the `~/xmonad-build` directory.

Let me know if you'd like further refinements!

---
The difference between building the `xmonad` binary with `cabal` and using `xmonad --recompile` lies in **scope**, **purpose**, and **build environment**:

---

### 1. **Building with `cabal`**
- **Purpose**: This is used to build the `xmonad` binary and its dependencies from source. It is typically done when setting up `xmonad` for the first time or when you want to build a clean, standalone binary.
- **Scope**: 
  - Builds the `xmonad` and `xmonad-contrib` libraries.
  - Produces a standalone `xmonad` binary that can be installed to a specific directory (e.g., `$HOME/.local/bin`).
  - Useful for creating a reusable or portable binary.
- **Environment**: 
  - Uses the `cabal` build system, which manages dependencies and builds in an isolated environment.
  - Requires a `cabal.project` file if you're building from local repositories.
- **Example**:
  ```bash
  cabal build xmonad
  cabal install xmonad --installdir=$HOME/.local/bin
  ```

---

### 2. **Using `xmonad --recompile`**
- **Purpose**: This is used to recompile your `xmonad.hs` configuration file after making changes to it. It ensures that your custom configuration is applied to the running `xmonad` instance.
- **Scope**:
  - Only recompiles the `xmonad.hs` file into a binary (usually stored in `~/.xmonad/xmonad-x86_64-linux`).
  - Does **not** rebuild the `xmonad` or `xmonad-contrib` libraries.
  - Limited to applying configuration changes, not building the entire `xmonad` project.
- **Environment**:
  - Relies on the `ghc` compiler to compile the `xmonad.hs` file.
  - Assumes that `xmonad` and its dependencies are already installed and available in your environment.
- **Example**:
  ```bash
  xmonad --recompile
  ```

---

### Key Differences
| Aspect                | `cabal build` / `cabal install`         | `xmonad --recompile`                  |
|-----------------------|-----------------------------------------|---------------------------------------|
| **Purpose**           | Build the `xmonad` binary and libraries | Recompile the `xmonad.hs` config file |
| **Scope**             | Full project build                     | Configuration-specific build          |
| **Dependencies**      | Builds and manages dependencies         | Assumes dependencies are pre-installed|
| **Output**            | Standalone binary in a specified path   | Binary in `~/.xmonad/`                |
| **Use Case**          | Initial setup or clean builds           | Applying configuration changes        |

---

### When to Use Each
- **Use `cabal build`**:
  - When setting up `xmonad` for the first time.
  - When you want a clean, standalone binary.
  - When updating or modifying the `xmonad` or `xmonad-contrib` libraries.

- **Use `xmonad --recompile`**:
  - When you modify your `xmonad.hs` configuration file.
  - When you want to quickly apply changes to your running `xmonad` instance.

---

### Summary
- `cabal build` is for building the `xmonad` binary and libraries from source.
- `xmonad --recompile` is for recompiling your configuration file to apply changes. Both are complementary, and you'll typically use `cabal` for initial setup and `xmonad --recompile` for day-to-day configuration updates.

---
Your idea of using a script for a fresh clone and build process is excellent for maintaining a clean and lightweight factory setup. Adding the clone directories to `.gitignore` while optionally keeping the .git directories for version control is a practical approach. Here's how you can implement it:

---

### 1. **Script for Fresh Clone and Build**
Create a script that automates the cloning, building, and installation process. This ensures a clean build every time.

```bash
#!/bin/bash
# filepath: ~/xmonad-build/build-xmonad.sh

set -e  # Exit on error

# Define directories
BUILD_DIR=~/xmonad-build
INSTALL_DIR=$HOME/.local/bin

# Clean up old build directory
echo "Cleaning up old build directory..."
rm -rf $BUILD_DIR

# Create a fresh build directory
echo "Creating fresh build directory..."
mkdir -p $BUILD_DIR && cd $BUILD_DIR

# Clone repositories
echo "Cloning xmonad and xmonad-contrib repositories..."
git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib

# Create cabal.project file
echo "Setting up cabal project..."
cat > cabal.project <<EOF
packages: ./xmonad ./xmonad-contrib
EOF

# Build and install xmonad
echo "Building and installing xmonad..."
cabal update
cabal build xmonad
cabal install xmonad --installdir=$INSTALL_DIR --overwrite-policy=always

# Ensure the install directory is in PATH
if ! echo $PATH | grep -q "$INSTALL_DIR"; then
  echo "Add the following to your shell configuration file:"
  echo "export PATH=$INSTALL_DIR:\$PATH"
fi

echo "Build and installation complete!"
```

Make the script executable:

```bash
chmod +x ~/xmonad-build/build-xmonad.sh
```

Run it whenever you need a fresh build:

```bash
~/xmonad-build/build-xmonad.sh
```

---

### 2. **Add Clone Directories to `.gitignore`**
If you want to avoid storing the cloned repositories in version control, add the following to your `.gitignore` file:

```plaintext
# Ignore build directory
xmonad-build/xmonad/
xmonad-build/xmonad-contrib/
```

This ensures that the cloned repositories are ignored by Git.

---

### 3. **Optionally Keep .git Directories**
If you want to retain the .git directories for version control (e.g., to pull updates), modify the script to exclude .git directories from cleanup:

```bash
rm -rf $BUILD_DIR/xmonad $BUILD_DIR/xmonad-contrib
```

This way, the .git directories remain intact, allowing you to update the repositories with `git pull` instead of recloning.

---

### Benefits of This Approach
1. **Clean and Lightweight**: The build directory is recreated fresh each time, avoiding clutter.
2. **Version Control Flexibility**: Keeping .git directories allows you to pull updates without recloning.
3. **Automation**: The script handles everything, reducing manual steps.
4. **Portable**: The setup can be easily replicated on other systems.

Let me know if you'd like further refinements!

---


---
To implement your idea of cloning the repositories only the first time and then performing a `git pull` on subsequent runs, you can modify your script to check if the repositories already exist. If they do, it will clean up the working directories (but keep the .git directories) and pull the latest changes instead of recloning.

Here’s the updated script:

```bash
#!/bin/bash

set -e  # Exit on error

# Define directories
BUILD_DIR=~/repos/xmonad-build
INSTALL_DIR=$HOME/.local/bin

# Create the build directory if it doesn't exist
mkdir -p $BUILD_DIR
cd $BUILD_DIR

# Clone or update repositories
echo "Checking xmonad and xmonad-contrib repositories..."
if [ -d "$BUILD_DIR/xmonad/.git" ]; then
  echo "xmonad repository exists. Pulling latest changes..."
  cd xmonad
  git reset --hard HEAD  # Clean up working directory
  git pull
  cd ..
else
  echo "Cloning xmonad repository..."
  git clone https://github.com/xmonad/xmonad
fi

if [ -d "$BUILD_DIR/xmonad-contrib/.git" ]; then
  echo "xmonad-contrib repository exists. Pulling latest changes..."
  cd xmonad-contrib
  git reset --hard HEAD  # Clean up working directory
  git pull
  cd ..
else
  echo "Cloning xmonad-contrib repository..."
  git clone https://github.com/xmonad/xmonad-contrib
fi

# Create cabal.project file
echo "Setting up cabal project..."
cat > cabal.project <<EOF
packages: ./xmonad ./xmonad-contrib
EOF

# Build and install xmonad
echo "Building and installing xmonad..."
cabal update
cabal build xmonad
cabal install xmonad --installdir=$INSTALL_DIR --overwrite-policy=always

# Ensure the install directory is in PATH
if ! echo $PATH | grep -q "$INSTALL_DIR"; then
  echo "Add the following to your shell configuration file:"
  echo "export PATH=$INSTALL_DIR:\$PATH"
fi

echo "Build and installation complete!"
```

---

### Key Changes:
1. **Check for Existing Repositories**:
   - If the `xmonad` or `xmonad-contrib` directories already exist and contain a .git directory, the script performs a `git pull` to fetch the latest changes.
   - If the repositories don’t exist, it clones them.

2. **Clean Working Directory**:
   - Before pulling changes, the script runs `git reset --hard HEAD` to ensure the working directory is clean.

3. **Keeps .git Directories**:
   - The .git directories are preserved, allowing the script to pull updates instead of recloning.

---

### Benefits:
- **Efficiency**: Avoids recloning repositories unnecessarily.
- **Clean Updates**: Ensures the working directories are clean before pulling changes.
- **Version Control**: Keeps .git directories intact for easy updates.

Let me know if you need further refinements!

---

Good question! To ensure that xmonad is found when you log in via a TTY and run `startx`, the `PATH` configuration in your .bashrc must be properly applied to the environment used by `startx`. Here's how to verify and ensure it works:

---

### **Why It Might Not Work**
- The .bashrc file is typically sourced only for **interactive** shell sessions. When you log in and run `startx`, the shell might not source .bashrc automatically.
- If `$HOME/.local/bin` is not in the `PATH` for non-interactive sessions, `startx` might fail to find xmonad.

---

### **Solution**
To ensure `$HOME/.local/bin` is always in your `PATH`, you should add the `export PATH=$HOME/.local/bin:$PATH` line to your `~/.profile` or `~/.bash_profile` instead of (or in addition to) .bashrc. These files are sourced for **login shells**, which includes TTY logins.

1. **Edit `~/.profile` or `~/.bash_profile`**:
   Open `~/.profile` (or `~/.bash_profile` if it exists) and add the following line:

   ```bash
   export PATH=$HOME/.local/bin:$PATH
   ```

2. **Ensure `~/.profile` is Sourced**:
   If you're using `~/.bash_profile`, make sure it sources `~/.profile` (this is standard on many systems). Add this line to `~/.bash_profile` if it's not already there:

   ```bash
   [ -f ~/.profile ] && . ~/.profile
   ```

3. **Test the Configuration**:
   - Log out of your TTY session.
   - Log back in via TTY.
   - Run `echo $PATH` to confirm that `$HOME/.local/bin` is in your `PATH`.
   - Run `which xmonad` to confirm that xmonad is found.

4. **Run `startx`**:
   If the above steps are correct, `startx` should now find xmonad without any issues.

---

### **Alternative: Explicitly Specify xmonad in `.xinitrc`**
If you want to be extra cautious, you can explicitly specify the full path to xmonad in your `~/.xinitrc` file:

```bash
#!/bin/bash
exec /home/holmen1/.local/bin/xmonad
```

This ensures that `startx` will always find xmonad, regardless of the `PATH`.

---

### **Summary**
- Add `export PATH=$HOME/.local/bin:$PATH` to `~/.profile` or `~/.bash_profile` for login shells.
- Test the `PATH` after logging in via TTY to ensure it includes `$HOME/.local/bin`.
- Optionally, specify the full path to xmonad in `~/.xinitrc` for extra reliability.

Let me know if you need help testing or tweaking this further! 😊