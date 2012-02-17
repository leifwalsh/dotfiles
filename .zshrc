################################################################################
# Settings
################################################################################

[[ -f /etc/profile ]] && . /etc/profile
[[ -f /etc/zsh/zprofile ]] && . /etc/zsh/zprofile
[[ -f ~/.profile ]] && . ~/.profile

# History options.
export HISTFILE=~/.zsh_history
export HISTSIZE=5000
export SAVEHIST=5000
setopt hist_ignore_dups # ignore same commands run twice+
setopt appendhistory    # don't overwrite history 
setopt sharehistory histappend # share history between open shells

# Generic shell options.
setopt nopromptcr     # don't add \n which overwrites cmds with no \n
setopt nobeep         # i hate beeps
setopt noautomenu     # don't cycle completions
setopt autocd         # change to dirs without cd
setopt autopushd      # push directories by default
setopt nocheckjobs    # don't warn me about bg processes when exiting
setopt nohup          # and don't kill them, either
setopt completeinword # not just at the end
setopt alwaystoend    # when complete from middle, move cursor
setopt promptsubst    # do varaible fu in prompt
setopt extendedglob   # Nice things like *~*.c globs all but .c files
setopt correctall     # more correction
setopt multios        # makes "foo <file1 <file2 >out1 >out2 |grep" possible
setopt list_ambiguous # stop completing at ambiguities

# Use emacs style editing
bindkey -e

################################################################################
# Aliases
################################################################################

# Aliases.
if [ "$TERM"x != dumbx ]
then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
fi
alias lsd='ls -d'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias l=ls

# dvorak/us
alias aoeu='setxkbmap us'
alias asdf='setxkbmap dvorak'

# sane readline behavior
export WORDCHARS='*?%^{}'

setopt extended_glob
preexec () {
    if [[ "$TERM" =~ "screen" ]]; then
    local CMD=${1[(wr)^(*=*|sudo|-*)]}
    echo -n "\ek$CMD\e\\"
    fi
}

################################################################################
# Completion
################################################################################

# export proper variables for ls and completion
if [[ -f /etc/DIR_COLORS ]]
then
  eval $(dircolors /etc/DIR_COLORS)
elif [[ -f ${HOME}/.dircolors ]]
then
  eval $(dircolors ${HOME}/.dircolors)
else
  eval $(dircolors -b)
fi
export ZLS_COLORS=${LS_COLORS}

# complete!
autoload -U compinit; compinit

# load colorizing modules
zmodload -a colors
zmodload -a autocomplete
zmodload -a complist

setopt dvorak

# cache stuff
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# add color
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# rehashing function
_force_rehash() {
  (( CURRENT == 1 )) && rehash
  return 1
}

# proper order
zstyle ':completion:*' completer _force_rehash _complete _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
# bold and underline sections
zstyle ':completion:*:descriptions' format '%U%d%u'
# mention errors
zstyle ':completion:*:corrections' format "%U%d%u (errors %e)"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
# ignore same file
zstyle ':completion:*:(rm|kill|killall|diff|tar|cat|zcat|ls|cp|mv|git):*' ignore-line yes
# for manpages
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes

# load up ssh hosts
[ -f ~/.ssh/config ] && : ${(A)ssh_config_hosts:=${${${${(@M)${(f)"$(<~/.ssh/config)"}:#Host *}#Host }:#*\**}:#*\?*}}
[ -f ~/.ssh/known_hosts ] && : ${(A)ssh_known_hosts:=${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*}}
zstyle ':completion:*:*:*' hosts $ssh_config_hosts $ssh_known_hosts
# colorize ssh hosts
zstyle ':completion:*:*:*:*:hosts' list-colors '=*=36'
zstyle ':completion:*:*:*:*:users' list-colors '=*=31'
# ignore lots of system users
zstyle ':completion:*:*:*:users' ignored-patterns \
    adm apache aspnet bin cron daemon dhcp distcc games gdm haldaemon halt \
    ident junkbust lp mail mailnull named news nfsnobody nobody nscd ntp \
    operator pcap polkituser portage postgres postmaster pulse radvd \
    rpc rpcuser rpm smmsp shutdown squid sshd sync uucp vcsa xfs backup bind \
    dictd gnats identd irc man messagebus postfix proxy sys www-data

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn bzr hg darcs
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' branchformat "%b:%r"
zstyle ':vcs_info:*:prompt:*' unstagedstr '-'
zstyle ':vcs_info:*:prompt:*' stagedstr '*'
zstyle ':vcs_info:*:prompt:*' actionformats "%F{blue}%2~ [%f%F{red}%c%u%f%F{magenta}%b%f%F{blue}|%f%F{yellow}%a%f%F{blue}]%f "
zstyle ':vcs_info:*:prompt:*' formats "%F{blue}%2~ [%f%F{red}%c%u%f%F{magenta}%b%f%F{blue}]%f "
# no git stuff for anything in my homedir
zstyle ':vcs_info:*:prompt:leif' actionformats "%F{blue}%2~%f "
zstyle ':vcs_info:*:prompt:leif' formats "%F{blue}%2~%f "
zstyle ':vcs_info:*:prompt:*' nvcsformats "%F{blue}%2~%f "

# show current directory in the menu bar
title () {
    case $TERM in
        screen*)
            print -nR $'\033k'$1$'\033'\\; print -nR $'\033]0;'$2$'\a'
            ;;
        xterm*|rxvt*)
            print -nR $'\033]0;'$2$'\a'
            ;;
    esac
}

precmd () {
    vcs_info prompt
    title zsh "[${TERM%-*}] zsh: ${(%):-%m@%n %~}"
}

preexec () {
    emulate -L zsh
    local -a cmd; cmd=(${(z)1})

    case $cmd[1] in
        fg)
            if (( $#cmd == 1)); then
                cmd=(builtin jobs -l %+)
            else
                cmd=(builtin jobs -l ${(Q)cmd[2]})
            fi;;
        %*) cmd=(builtin jobs -l ${(Q)cmd[1]});;
        sudo|exec) shift cmd;&
        *) title $cmd[1]:t "[${TERM%-*}] zsh: $cmd[1]:t $cmd[2,-1]"
            return;;
    esac

    local -A jt; jt=(${(kv)jobtexts})
    $cmd >>(read num rest
        cmd=(${(z)${(e):-\$jt$num}})
        title $cmd[1]:t "[${TERM%-*}] zsh: $cmd[1]:t $cmd[2,-1]") 2>/dev/null
}

if [[ $TERM == "dumb" && $EMACS == "t" ]]; then
    # emacs is dumb
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    PROMPT="%m %2~ %# "
else
    PROMPT='%(#.%F{red}%B%m%b%f.%F{green}%m%f) ${vcs_info_msg_0_}%F{blue}%#%f '
    #RPROMPT="%F{blue}%(?..(%f%F{red}%?%f%F{blue}%) )%B[%b%f%F{yellow}%T%f%F{blue}%B]%b%f"
fi

# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _approximate _prefix
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/Users/leif/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
