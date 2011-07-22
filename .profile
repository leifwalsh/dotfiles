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

export PATH=$HOME/bin:/usr/local/bin:$PATH
export EDITOR="emacsclient -c"

# start ssh-agent
eval $(keychain --agents ssh -q --eval id_rsa)
