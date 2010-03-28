export CLOJURE_EXT=$HOME/.clojure
export PATH=$PATH:$HOME/bin:$HOME/.gem/ruby/1.8/bin:$HOME/git/clojure-contrib/launchers/bash
alias clj=clj-env-dir

# start ssh-agent
eval $(keychain -q --eval ${HOME}/.ssh/id_rsa)
