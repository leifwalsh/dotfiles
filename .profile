# start up a master ssh connection to tokubuntu
if ps aux | grep VirtualBox | grep 'Ubuntu 64' | grep -v grep &>/dev/null
then
    if ! ps aux | grep 'ssh -Y -C' | grep -v grep &>/dev/null
    then
        ssh -Y -C -o ServerAliveInterval=30 -fN tokubuntu &>/dev/null
    fi
fi


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
