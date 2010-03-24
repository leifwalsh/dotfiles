export PATH=$PATH:$HOME/bin:$HOME/.gem/ruby/1.8/bin

# start ssh-agent
eval $(keychain -q --eval ${HOME}/.ssh/id_rsa)
