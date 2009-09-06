export PYTHONSTARTUP="$HOME/.pythonrc.py"
export PATH=$PATH:$HOME/bin

# start ssh-agent
eval $(keychain -q --eval ${HOME}/.ssh/id_rsa)
