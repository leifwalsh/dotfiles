#!/bin/bash

set -e

sudo apt-get install --no-install-recommends --ignore-missing --quiet --assume-yes zsh
chsh --shell /usr/bin/zsh
