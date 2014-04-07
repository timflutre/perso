# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs
export PATH=$HOME/bin:$PATH
export CFLAGS="$CFLAGS -I$HOME/include"
export CPPFLAGS="$CPPFLAGS -I$HOME/include"
export CXXFLAGS="$CXXFLAGS -I$HOME/include"
export LDFLAGS="$LDFLAGS -L$HOME/lib"

export MANPATH=$HOME/share/man:$MANPATH

export R_LIBS_USER=$HOME/src_ext/Rlibs
export PYTHONPATH=$HOME/lib/python/:$PYTHONPATH

# http://perlgeek.de/en/article/set-up-a-clean-utf8-environment
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# grant read permission to group members
if [ "$HOSTNAME" == "marmadais" ]; then
    umask u=rwx,g=rx,o=
fi
