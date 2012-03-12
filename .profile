if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

if which brew &>/dev/null && [ -d $(brew --prefix coreutils)/libexec/gnubin ]; then
    export PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
fi

export PATH=$HOME/bin:$HOME/local/bin:$HOME/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
export LD_LIBRARY_PATH=$HOME/local/lib:/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export INFOPATH=$HOME/local/share/info:$INFOPATH
export PYTHONPATH=$HOME/local/lib/python2.7/site-packages/:$PYTHONPATH
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="vim"

if [[ $TERM = "xterm" ]]
then
    export TERM=xterm-256color
fi
if [[ $TERM = "rxvt-unicode" ]]
then
    export TERM=xterm
fi

if pgrep polipo &>/dev/null
then
    export all_proxy=localhost:8123
    export http_proxy=$all_proxy
    export https_proxy=$all_proxy
    export ftp_proxy=$all_proxy
fi

# add xcode 4.2 developer path
if [[ -d /Developer/usr/bin ]]
then
    export PATH=$PATH:/Developer/usr/bin
fi

# start ssh-agent
if which keychain &>/dev/null
then
    eval $(keychain --agents ssh -q --eval id_rsa)
fi
