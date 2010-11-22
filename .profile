# cgroup stuff
if [ "$PS1" ] ; then 
    mkdir -m 0700 /dev/cgroup/cpu/user/$$
    echo $$ > /dev/cgroup/cpu/user/$$/tasks
fi

export CLOJURE_EXT=$HOME/.clojure:$HOME/.clojure/ext
export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/.gem/ruby/1.8/bin:/var/lib/gems/1.8/bin:$HOME/git/clojure-contrib/launchers/bash
alias clj=clj-env-dir
export MANPATH=:$HOME/svn/scala-2.8.1_RC1/build/scaladoc/manual/man

# start ssh-agent
eval $(keychain -q)