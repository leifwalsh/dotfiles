if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

export CLOJURE_EXT=$HOME/.clojure:$HOME/.clojure/ext
export PATH=/usr/local/bin:$PATH:$HOME/bin
alias clj=clj-env-dir
export MANPATH=:$HOME/svn/scala-2.8.1_RC1/build/scaladoc/manual/man

# start ssh-agent
eval $(keychain --agents ssh -q --eval id_rsa)
