export PATH=$HOME/bin:$HOME/local/bin:/usr/local/bin:$PATH

# start ssh-agent
eval $(keychain -q --eval id_rsa)
