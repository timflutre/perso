# -*-Shell-script-*-
# Tim's .bashrc
# Versioning: https://github.com/timflutre/perso

export NAME="Timothée Flutre" # used in .emacs
export COMPUTERNAME="<to_be_filled>" # used in .bash_profile, .Rprofile, ...

# http://perlgeek.de/en/article/set-up-a-clean-utf8-environment
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export CFLAGS="$CFLAGS -I$HOME/include"
export CPPFLAGS="$CPPFLAGS -I$HOME/include"
export CXXFLAGS="$CXXFLAGS -I$HOME/include"
export LDFLAGS="$LDFLAGS -L$HOME/lib"


if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# need to set PS1 here so that emacs term-mode can have it
# https://www.digitalocean.com/community/tutorials/how-to-customize-your-bash-prompt-on-a-linux-vps
PS1="\u@\h:\W\$ "
