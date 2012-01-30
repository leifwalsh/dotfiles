if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

if [ -f /usr/local/Cellar/coreutils/8.14/aliases ]; then
  source /usr/local/Cellar/coreutils/8.14/aliases
fi

export PATH=$HOME/bin:$HOME/local/bin:$HOME/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
export LD_LIBRARY_PATH=$HOME/local/lib:/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export INFOPATH=$HOME/local/share/info:$INFOPATH
export PYTHONPATH=$HOME/local/lib/python2.7/site-packages/:$PYTHONPATH
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="vim"

# start ssh-agent
if which keychain &>/dev/null
then
    eval $(keychain --agents ssh -q --eval id_rsa)
fi
