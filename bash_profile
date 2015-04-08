# -*-Shell-script-*-
# Tim's configuration file for Bash

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
COMPUTERNAME="to_be_filled"
if [ "$COMPUTERNAME" == "laptop-pro" ]; then
  export PATH=/usr/local/texlive/2014/bin/x86_64-linux:$PATH
  export MANPATH=/usr/local/texlive/2014/texmf-dist/doc/man:$MANPATH
  export INFOPATH=/usr/local/texlive/2014/texmf-dist/doc/info:$INFOPATH
fi
if [ "$COMPUTERNAME" == "southgreen" ]; then
  module load system/emacs/24.4
  module load compiler/gcc/4.9.2
  module load bioinfo/xclip/0.12
  module load system/python/2.7.9
  module load bioinfo/gdal/1.9.2
  module load bioinfo/geos/3.4.2
  module load bioinfo/R/3.1.3
  module load system/java/jre6
  module load bioinfo/FastQC/0.11.2
  
  # http://stackoverflow.com/a/4454754/597069
  export GIT_SSL_NO_VERIFY=true
  
  export PATH=/usr/local/jdk/bin:/usr/local/jre/bin:$PATH
  
  export PYTHONPATH=$HOME/lib/python2.7/site-packages/:$PYTHONPATH
fi
if [ "$COMPUTERNAME" == "urgi" ]; then
  # http://stackoverflow.com/a/4454754/597069
  export GIT_SSL_NO_VERIFY=true
fi
if [ "$COMPUTERNAME" == "midway" ]; then
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
