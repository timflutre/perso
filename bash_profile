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

# Machine specific environment
export CLUSTERNAME="to_be_filled"
if [ "$CLUSTERNAME" == "southgreen" ]; then
    # load env for R-3.1.0 compiled with gcc 4.8.2
    . /etc/profile.d/modules.sh
    module load compiler/gcc-4.8.2
    
    # grant no permission to anyone except me
    umask u=rwx,g=,o=
    
    # http://stackoverflow.com/a/4454754/597069
    export GIT_SSL_NO_VERIFY=true
    
    export PATH=/usr/local/jre/bin:/usr/local/bioinfo/fastqc_v0.11.2:$PATH
fi
if [ "$CLUSTERNAME" == "urgi" ]; then
    # http://stackoverflow.com/a/4454754/597069
    export GIT_SSL_NO_VERIFY=true
fi
if [ "$CLUSTERNAME" == "midway" ]; then
    module load gcc
    module load R/3.0
    module load git
    module load coreutils
    module load texinfo
    module load texlive
    module load autoconf
    module load automake
    module load libtool
    module load zlib
    module load gsl
    module load python
    module load parallel
    export PATH=$HOME/bin:$PATH
fi
