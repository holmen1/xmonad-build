#!/bin/bash

set -e  # Exit on error

# Define directories
BUILD_DIR=~/repos/xmonad-build
INSTALL_DIR=$HOME/.local/bin

# Create the build directory if it doesn't exist
mkdir -p $BUILD_DIR
cd $BUILD_DIR

# Function to clone or update a repository
update_repo() {
  local repo_name=$1
  local repo_url=$2

  [ -d "$BUILD_DIR/$repo_name/.git" ] && {
    echo "Updating $repo_name repository..."
    git -C "$BUILD_DIR/$repo_name" reset --hard HEAD  # Clean up working directory
    git -C "$BUILD_DIR/$repo_name" pull
    return
  }

  echo "Cloning $repo_name repository..."
  git clone "$repo_url" "$BUILD_DIR/$repo_name"
}

# Update or clone xmonad and xmonad-contrib repositories
update_repo "xmonad" "https://github.com/xmonad/xmonad"
update_repo "xmonad-contrib" "https://github.com/xmonad/xmonad-contrib"

# Build and install xmonad and xmonad-contrib
echo "Building and installing xmonad and xmonad-contrib..."
cabal update
# installs the libraries for recompilation
cabal install --installdir=$INSTALL_DIR --overwrite-policy=always --package-env=$HOME/.config/xmonad --lib xmonad xmonad-contrib
# installs the binary for execution
cabal install --installdir=$INSTALL_DIR --overwrite-policy=always --package-env=$HOME/.config/xmonad xmonad


# Ensure the install directory is in PATH
if ! echo $PATH | grep -q "$INSTALL_DIR"; then
  echo "Add the following to your shell configuration file:"
  echo "export PATH=$INSTALL_DIR:\$PATH"
fi

echo "Build and installation complete!"