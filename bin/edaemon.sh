#!/bin/bash

pgrep emacs || exec emacs --daemon
