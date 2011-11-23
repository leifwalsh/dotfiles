# start up a master ssh connection to tokubuntu
if ps aux | grep VirtualBox | grep 'Ubuntu 64' | grep -v grep &>/dev/null
then
    if ! ps aux | grep 'ssh -Y -C' | grep -v grep &>/dev/null
    then
        nohup ssh -Y -C -o ServerAliveInterval=30 -fN tokubuntu &>/dev/null
    fi
fi

if [ -f ${HOME}/.termcap ]; then
  TERMCAP=$(< ${HOME}/.termcap)
  export TERMCAP
fi

export PATH=$HOME/bin:$HOME/local/bin:$HOME/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
export PYTHONPATH=/Users/leif/local/lib/python2.7/site-packages/:$PYTHONPATH
export EDITOR="emacsclient -t"
export ALTERNATE_EDITOR="vim"

# start ssh-agent
eval $(keychain --agents ssh -q --eval id_rsa)
