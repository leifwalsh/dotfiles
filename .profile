export CLOJURE_EXT=$HOME/.clojure:$HOME/.clojure/ext
export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/.gem/ruby/1.8/bin:/var/lib/gems/1.8/bin:$HOME/git/clojure-contrib/launchers/bash
alias clj=clj-env-dir

# start ssh-agent
eval $(keychain -q)
