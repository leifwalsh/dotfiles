#!/bin/sh

# -e: exit on error
# -u: exit on unset variables
set -eu

if ! chezmoi="$(command -v chezmoi)"; then
  bin_dir="${HOME}/.local/bin"
  chezmoi="${bin_dir}/chezmoi"
  echo "Installing chezmoi to '${chezmoi}'" >&2
  if command -v curl >/dev/null; then
    chezmoi_install_script="$(curl -fsSL https://chezmoi.io/get)"
  elif command -v wget >/dev/null; then
    chezmoi_install_script="$(wget -qO- https://chezmoi.io/get)"
  else
    echo "To install chezmoi, you must have curl or wget installed." >&2
    exit 1
  fi
  sh -c "${chezmoi_install_script}" -- -b "${bin_dir}"
  unset chezmoi_install_script bin_dir
fi

# POSIX way to get script's dir: https://stackoverflow.com/a/29834779/12156188
script_dir="$(cd -P -- "$(dirname -- "$(command -v -- "$0")")" && pwd -P)"

"${chezmoi}" init --apply --source="${script_dir}"

if ! nix_env="$(command -v nix-env)"; then
  if ! command -v xz; then
    if ! apt_get="$(command -v apt-get)"; then
      echo "Don't know how to install xz without apt-get" >&2
      exit 1
    fi
    sudo apt-get update
    export DEBIAN_FRONTEND=noninteractive
    sudo apt-get -y install --no-install-recommends xz-utils
  fi
  sudo install -d -m755 -o $(id -u) -g $(id -g) /nix

  export NIX_INSTALLER_NO_MODIFY_PROFILE=1
  if command -v curl >/dev/null; then
    curl -L "https://nixos.org/nix/install" | sh
  elif command -v wget >/dev/null; then
    wget -qO- "https://nixos.org/nix/install" | sh
  else
    echo "To install nix, you must have curl or wget installed." >&2
    exit 1
  fi

  nix_env="${HOME}/.nix-profile/bin/nix-env"
fi

"${nix_env}" -if "${HOME}/.devenv.nix"

"${HOME}/.nix-profile/bin/tldr" --update
