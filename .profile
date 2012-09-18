# add xcode 4.4 developer path
tcpath=/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr
if [[ -d $tcpath ]]
then
    PATH=$PATH:$tcpath/bin
    MANPATH=$MANPATH:$tcpath/share/man
fi
platpath=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.8.sdk/usr
if [[ -d $platpath ]]
then
    #PATH=$PATH:$platpath/bin
    C_INCLUDE_PATH=$C_INCLUDE_PATH:$platpath/include
    CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:$platpath/include
fi
devpath=/Applications/Xcode.app/Contents/Developer/usr
if [[ -d $devpath ]]
then
    PATH=$PATH:$devpath/bin
    MANPATH=$MANPATH:$devpath/share/man
fi

# add coreutils
if which brew &>/dev/null && [ -d $(brew --prefix coreutils)/libexec/gnubin ]; then
    PATH=$(brew --prefix coreutils)/libexec/gnubin:$PATH
fi

if [[ -d $HOME/local/plan9 ]]
then
    PLAN9=$HOME/local/plan9
    export PLAN9
    PATH=$PATH:$PLAN9/bin
    MANPATH=$MANPATH:$PLAN9/man
fi

PATH=$HOME/bin:$HOME/local/bin:$HOME/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
PKG_CONFIG_PATH=$HOME/local/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
INFOPATH=$HOME/local/share/info:/usr/local/share/info:$INFOPATH
MANPATH=$HOME/local/share/man:/usr/local/share/man:$MANPATH
PYTHONPATH=$HOME/local/lib64/python:$HOME/local/lib/python2.7/site-packages:$PYTHONPATH

EDITOR="emacsclient -t"
ALTERNATE_EDITOR="vim"

# colorize less!
export LESS="-R"

if [[ $TERM = "xterm" ]]
then
    export TERM=xterm-256color
fi

# start ssh-agent
if which keychain &>/dev/null
then
    eval $(keychain --agents ssh -q --eval id_rsa)
fi

export PATH C_INCLUDE_PATH CPLUS_INCLUDE_PATH PKG_CONFIG_PATH INFOPATH MANPATH PYTHONPATH EDITOR ALTERNATE_EDITOR
