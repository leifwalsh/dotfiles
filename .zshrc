################################################################################
# Settings
################################################################################

source /etc/profile
source ~/.profile

# History options.
export HISTFILE=~/.zsh_history
export HISTSIZE=5000
export SAVEHIST=5000
setopt hist_ignore_dups # ignore same commands run twice+
setopt appendhistory    # don't overwrite history 

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
#setopt menucomplete   # Don't stop completing at ambiguities

# Use emacs style editing
bindkey -e

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
alias apt='sudo aptitude'
alias apu='sudo aptitude update'
alias api='sudo aptitude install'

################################################################################
# Prompt jazz
################################################################################

function hashcolor4 {
    echo $1 | md5sum | python -c "
import sys
x = list(sys.stdin.read())
c = 'black red green yellow blue magenta cyan white default'.split()
n = -1
while n not in range(len(c)):
    try:
        n = ord(x.pop(0)) - ord('a')
    except IndexError:
        n = 5
        break
print c[n]"
}
# alternately, but doesn't gracefully handle small color ranges
function hashcolor2 {
	c=(black red green yellow blue magenta cyan white default)
	echo $c[`hostname | md5sum | cut -b1 | tr a-f 0-5`]
}

#autoload -U promptinit; promptinit
#if [[ $HOST == 'dash' ]]; then
#    prompt adam2 gray white cyan cyan
#elif [[ $HOST == 'howl' ]]; then
#    #prompt adam2 8bit green green green
#    prompt adam2 green green green
#else;
#    prompt adam2 $(hashcolor4 $HOST) white white
#fi

# Path for my goodies.
export PATH=/sbin:/usr/sbin:/usr/local/sbin:/usr/X11R6/bin:~/bin:$PATH

# Perforce
if [[ -e $HOME/.p4config ]]; then
	source ~/.bashrc
fi

################
# Phil's ZSH prompt
##############
function precmd {

    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))


    ###
    # Truncate the path if it's too long.
    
    PR_FILLBAR=""
    PR_PWDLEN=""
    
    local promptsize=${#${(%):---(%n@%m:%l)---()--}}
    local pwdsize=${#${(%):-%~}}
    
    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
        ((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
    PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi


    ###
    # Get APM info.

#    if which ibam > /dev/null; then
#    PR_APM_RESULT=`ibam --percentbattery`
#    elif which apm > /dev/null; then
#    PR_APM_RESULT=`apm`
#    fi
    PR_APM_RESULT=
}


setopt extended_glob
preexec () {
    if [[ "$TERM" == "screen" ]]; then
    local CMD=${1[(wr)^(*=*|sudo|-*)]}
    echo -n "\ek$CMD\e\\"
    fi
}


function hashcolor {
    # Hash function mostly used to colorize the hbar in the prompt
    # based on the host name
    echo $1 | md5sum | python -c "
import sys
x = list(sys.stdin.read())
c = 'PR_RED PR_GREEN PR_YELLOW PR_BLUE PR_MAGENTA PR_CYAN PR_WHITE'.split()
n = -1
while n not in range(len(c)):
    try:
        n = ord(x.pop(0)) - ord('a')
    except IndexError:
        n = 5
        break
print c[n]"
}

setprompt () {
    ###
    # Need this so the prompt will work.

    setopt prompt_subst


    ###
    # See if we can use colors.

    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"


    ###
    # See if we can use extended characters to look nicer.
    
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}

    
    ###
    # Decide if we need to set titlebar text.
    
    case $TERM in
    xterm*)
        PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
        ;;
    screen)
        PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
        ;;
    *)
        PR_TITLEBAR=''
        ;;
    esac
    
    
    ###
    # Decide whether to set a screen title
    if [[ "$TERM" == "screen" ]]; then
    PR_STITLE=$'%{\ekzsh\e\\%}'
    else
    PR_STITLE=''
    fi
    
    
    ###
    # APM detection
    
#    if which ibam > /dev/null; then
#    PR_APM='$PR_RED${${PR_APM_RESULT[(f)1]}[(w)-2]}%%(${${PR_APM_RESULT[(f)3]}[(w)-1]})$PR_LIGHT_BLUE:'
#    elif which apm > /dev/null; then
#    PR_APM='$PR_RED${PR_APM_RESULT[(w)5,(w)6]/\% /%%}$PR_LIGHT_BLUE:'
#    else
    PR_APM=''
#    fi
    
    
    ###
    # Finally, the prompt.
    PR_HBAR_COLOR='$'$(hashcolor $HOST)

    PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_CYAN$PR_SHIFT_IN$PR_ULCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%(!.%SROOT%s.%n)$PR_GREEN@%m:%l\
$PR_BLUE)$PR_SHIFT_IN$PR_HBAR'$PR_HBAR_COLOR'$PR_HBAR${(e)PR_FILLBAR}$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
$PR_MAGENTA%$PR_PWDLEN<...<%~%<<\
$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_URCORNER$PR_SHIFT_OUT\

$PR_CYAN$PR_SHIFT_IN$PR_LLCORNER$PR_BLUE$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_LIGHT_RED%?$PR_BLUE:)\
${(e)PR_APM}$PR_YELLOW%D{%H:%M}\
$PR_LIGHT_BLUE:%(!.$PR_RED.$PR_WHITE)%#$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_NO_COLOUR '

    RPROMPT=' $PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_BLUE$PR_HBAR$PR_SHIFT_OUT\
($PR_YELLOW%D{%a,%b%d}$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_CYAN$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

    PS2='$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_BLUE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_LIGHT_GREEN%_$PR_BLUE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_CYAN$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '
}

################################################################################
# Completion
################################################################################

[ -f ~/.ssh/config ] && : ${(A)ssh_config_hosts:=${${${${(@M)${(f)"$(<~/.ssh/config)"}:#Host *}#Host }:#*\**}:#*\?*}}
[ -f ~/.ssh/known_hosts ] && : ${(A)ssh_known_hosts:=${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*}}
zstyle ':completion:*:*:*' hosts $ssh_config_hosts $ssh_known_hosts

# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' '' '' 'r:|[._-]=* r:|=*'
zstyle :compinstall filename '/home/leif/.zshrc'

autoload -U promptinit
promptinit
if [ "$TERM"x != dumbx -a "$TERM"x != linuxx ]
then
    setprompt
    #prompt fire red blue green white white white
else
    prompt redhat
fi
autoload -Uz compinit
compinit
# End of lines added by compinstall

fortune
