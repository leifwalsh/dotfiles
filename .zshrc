################################################################################
# Settings
################################################################################

. /etc/zsh/zprofile
. ~/.profile

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

# Fix moving/killing by words

_my_extended_wordchars='*?_-.[]~=&;!#$%^(){}<>:@,\\';
WORDCHARS=${_my_extended_wordchars}
#'
_my_extended_wordchars_space="${my_extended_wordchars} "
_my_extended_wordchars_slash="${my_extended_wordchars}-/"

# is the current position \-quoted ?
function _is_quoted(){
    test "${BUFFER[$CURSOR-1,CURSOR-1]}" = "\\"
}

_unquote-backward-delete-word(){
    while  _is_quoted
    do zle .backward-kill-word
    done
}

_unquote-forward-delete-word(){
    while  _is_quoted
    do zle .kill-word
    done
}

_unquote-backward-word(){
    while  _is_quoted
    do zle .backward-word
    done
}

_unquote-forward-word(){
    while _is_quoted
    do zle .forward-word
    done
}

_backward-delete-to-space() {
    local WORDCHARS=${_my_extended_wordchars_slash}
    zle .backward-kill-word
    _unquote-backward-delete-word
}

_backward-delete-to-/ () {
    local WORDCHARS=${_my_extended_wordchars}
    zle .backward-kill-word
    _unquote-backward-delete-word
}

_forward-delete-to-space() {
    local WORDCHARS=${_my_extended_wordchars_slash}
    zle .kill-word
    _unquote-forward-delete-word
}

_forward-delete-to-/ () {
    local WORDCHARS=${_my_extended_wordchars}
    zle .kill-word
    _unquote-forward-delete-word
}

_backward-to-space() {
    local WORDCHARS=${_my_extended_wordchars_slash}
    zle .backward-word
    _unquote-backward-word
}

_forward-to-space() {
    local WORDCHARS=${_my_extended_wordchars_slash}
    zle .forward-word
    _unquote-forward-word
}

_backward-to-/ () {
    local WORDCHARS=${_my_extended_wordchars}
    zle .backward-word
    _unquote-backward-word
}

_forward-to-/ () {
    local WORDCHARS=${_my_extended_wordchars}
    zle .forward-word
    _unquote-forward-word
}

zle -N _backward-delete-to-/
zle -N _forward-delete-to-/
zle -N _backward-delete-to-space
zle -N _forward-delete-to-space
zle -N _backward-to-/
zle -N _forward-to-/
zle -N _backward-to-space
zle -N _forward-to-space
#bindkey '^w'        _backward-delete-to-/
#bindkey '^[^w'      _backward-delete-to-space
#bindkey "^[^[[D"    _backward-to-/
#bindkey "^[^[[C"    _forward-to-/

#bindkey "^[b"       _backward-to-/
bindkey "^[^b"      _backward-to-space

#bindkey "^[f"       _forward-to-/
bindkey "^[^f"      _forward-to-space

#bindkey "\M\b"      _backward-delete-to-/
#bindkey "^\b"       _backward-delete-to-/

# C-Backspace and C-M-Backspace
if [[ $TERM =~ "(xterm|screen)*" ]]; then
    bindkey ""        backward-kill-word
    bindkey "\e"     _backward-delete-to-space
else
    bindkey "^\b"       backward-kill-word
    bindkey "^[^\b"     _backward-delete-to-space
fi

bindkey "^[^d"      _forward-delete-to-space

# Delete, Home, End, arrow keys (udlr)
if [[ $TERM =~ "(xterm|screen)*" ]]; then
    bindkey '\2333~'    delete-char
    bindkey '\2333;5~'  _forward-delete-to-/
    bindkey '\2333;3~'  _forward-delete-to-/
    bindkey '\233H'     beginning-of-line
    bindkey '[H'      beginning-of-line
    bindkey '\233F'     end-of-line
    bindkey '[F'      end-of-line
    bindkey '\233D'     backward-char
    bindkey '\233C'     forward-char
    bindkey '\233A'     up-history
    bindkey '\233B'     down-history
else
    bindkey '[3~'     delete-char
    bindkey '[7~'     beginning-of-line
    bindkey '[8~'     end-of-line
fi

#bindkey "\M^?"      _forward-delete-to-/
#bindkey "^^?"       _forward-delete-to-/
#bindkey "^[^^?"     _forward-delete-to-space

################################################################################
# Aliases
################################################################################

# Global aliases; expanded anywhere on the line.
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g CA="2>&1 | cat -A"
alias -g C='| wc -l'
alias -g G='| egrep'
alias -g H='| head'
alias -g M='| most'
alias -g L='| less'
alias -g MM="2>&1 | most"
alias -g N="> /dev/null 2>&1"
alias -g V="| vim -"

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

# Shortcut aliases
alias f='find 2>/dev/null | grep'
alias g='grep -d recurse'
alias m=most
alias p='ps -aux'

# dvorak/us
alias aoeu='setxkbmap us'
alias asdf='setxkbmap dvorak'

# Debian specific aliases
#alias apt='sudo aptitude'
#alias apu='sudo aptitude update'
#alias api='sudo aptitude install'

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
eval $(dircolors /etc/DIR_COLORS)
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
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
# mention errors
zstyle ':completion:*:corrections' format "%U%B%d%b%u (errors %e)"
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
zstyle ':vcs_info:*:prompt:*' actionformats "%B%F{blue}%2~ [%f%F{red}%c%u%f%F{magenta}%b%f%F{blue}|%f%F{yellow}%a%f%F{blue}]%f%%b"
zstyle ':vcs_info:*:prompt:*' formats "%B%F{blue}%2~ [%f%F{red}%c%u%f%F{magenta}%b%f%F{blue}]%f%%b"
# no git stuff for anything in my homedir
zstyle ':vcs_info:*:prompt:leif' actionformats "%B%F{blue}%2~%f%%b"
zstyle ':vcs_info:*:prompt:leif' formats "%B%F{blue}%2~%f%%b"
zstyle ':vcs_info:*:prompt:*' nvcsformats "%B%F{blue}%2~%f%b"

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
    PROMPT="%n@%m %2~ %# "
else
    PROMPT='%B%(#.%F{red}%m%f.%F{green}%n@%m%f)%b ${vcs_info_msg_0_} %B%F{blue}%#%f%b '
    RPROMPT="%B%F{blue}%(?..(%f%F{red}%?%f%F{blue}%) )[%f%F{yellow}%T%f%F{blue}]%f%b"
fi

# done setting up

fortune
