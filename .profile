PATH=/usr/local/bin:/usr/local/sbin:$PATH

# add xcode 4.2 developer path
if [[ -d /Developer/usr/bin ]]
then
    PATH=$PATH:/Developer/usr/bin
fi

# add coreutils
if which brew &>/dev/null && [ -d $(brew --prefix coreutils)/libexec/gnubin ]; then
    PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
fi

PATH=$HOME/local/bin:$HOME/local/sbin:$PATH
PATH=$HOME/bin:$PATH

LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
LD_LIBRARY_PATH=$HOME/local/lib:$LD_LIBRARY_PATH

PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
PKG_CONFIG_PATH=$HOME/local/lib:$PKG_CONFIG_PATH

INFOPATH=$HOME/local/share/info:$INFOPATH

MANPATH=$HOME/local/share/man:$MANPATH

PYTHONPATH=$HOME/local/lib64/python:$HOME/local/lib/python2.7/site-packages:$PYTHONPATH

EDITOR="emacsclient -t"
ALTERNATE_EDITOR="vim"

if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

if [[ $TERM = "xterm" ]]
then
    export TERM=xterm-256color
elif [[ $TERM = "rxvt-unicode" ]]
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

# start ssh-agent
if which keychain &>/dev/null
then
    eval $(keychain --agents ssh -q --eval id_rsa)
fi

export PATH LD_LIBRARY_PATH PKG_CONFIG_PATH INFOPATH MANPATH PYTHONPATH EDITOR ALTERNATE_EDITOR
