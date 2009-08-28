export PYTHONSTARTUP="$HOME/.pythonrc.py"

# start ssh-agent
eval $(keychain -q --eval ${HOME}/.ssh/id_rsa)
