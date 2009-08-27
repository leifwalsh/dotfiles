# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export PYTHONSTARTUP="$HOME/.pythonrc.py"

# ssh-agent stuff

SSH_ENV="$HOME/.ssh/environment"

function start_agent {
     echo -n Initialising new SSH agent...
     /usr/bin/ssh-agent | sed 's/^echo/#echo/' >"$SSH_ENV"
     echo done.
     chmod 600 "$SSH_ENV"
     . "$SSH_ENV" >/dev/null
     /usr/bin/ssh-add;
}

# Source SSH settings, if applicable
if [ -f "$SSH_ENV" ]; then
     . "$SSH_ENV" > /dev/null
     ps -ef | grep $SSH_AGENT_PID | grep ssh-agent$ > /dev/null || {
         start_agent;
     }
else
     start_agent;
fi
