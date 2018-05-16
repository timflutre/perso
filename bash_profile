# -*-Shell-script-*-
# Tim's .bash_profile
# Versioning: https://github.com/timflutre/perso

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

# Machine specific environment (define COMPUTERNAME in .bashrc)
if [ "$COMPUTERNAME" == "laptop-pro" ]; then
  PATH=/usr/local/texlive/2014/bin/x86_64-linux:$PATH
  MANPATH=/usr/local/texlive/2014/texmf-dist/doc/man:$MANPATH
  INFOPATH=/usr/local/texlive/2014/texmf-dist/doc/info:$INFOPATH
fi
if [ "$COMPUTERNAME" == "southgreen" ]; then
  # module load compiler/glibc/2.14 # used by GEMMA v0.96 static bin
  module load compiler/gcc/4.9.2 # used by many softwares
  # module load compiler/gcc/6.3.0 # used by GEMMA v0.97
  module load system/zlib/1.2.8
  module load system/emacs/24.4
  module load system/git/2.8.3
  module load system/python/2.7.9
  # module load system/java/jdk6 # caution: conflict with jr6 and jr7
  # module load system/java/jre6
  # module load system/java/jre7 # caution: conflict with R package XLConnect
  module load system/java/jre8 # required by FastQC
  module load system/boost/1_58_0
  module load mpi/openmpi/1.6.5
  # module load bioinfo/ARCAD/1
  module load bioinfo/bcftools/1.8
  module load bioinfo/beagle/4.1
  # module load bioinfo/beagle-lib/20150321
  module load bioinfo/bedtools/2.24.0
  module load bioinfo/bmagwa/2.0
  module load bioinfo/carthagene/1.3
  module load bioinfo/cutadapt/1.8.1
  module load bioinfo/datamash/1.3
  module load bioinfo/FastQC/0.11.2
  module load bioinfo/fastStructure/1.0
  module load bioinfo/GATK/3.7-0
  module load bioinfo/gdal/2.2.4
  # module load bioinfo/gemma/0.97 # requires gcc/6.3.0
  module load bioinfo/geos/3.4.2
  module load bioinfo/gs3/20160920
  module load bioinfo/htslib/1.8
  module load bioinfo/insilicut/1.0.0
  module load bioinfo/MUMmer/3.23
  module load bioinfo/ncbi-blast/2.2.30
  module load bioinfo/OpenBLAS/0.2.18
  module load bioinfo/OpenBUGS/3.2.3
  module load bioinfo/patman/1.2
  module load bioinfo/picard-tools/1.130
  module load bioinfo/plink/1.90b3v
  module load bioinfo/prank/v.150803
  module load bioinfo/samtools/1.3
  module load bioinfo/shapeIT/2.r837
  module load bioinfo/R/3.4.3
  module load bioinfo/vcftools/0.1.14_zlib-1.2.8
  module load bioinfo/xclip/0.12
  
  # http://stackoverflow.com/a/4454754/597069
  export GIT_SSL_NO_VERIFY=true
  
  PATH=/usr/local/jdk/bin:/usr/local/jre/bin:/homedir/flutre/texlive/bin/x86_64-linux:$PATH
  
  MANPATH=/home/flutre/texlive/texmf-dist/doc/man:$MANPATH
  INFOPATH=/home/flutre/texlive/texmf-dist/doc/info:$INFOPATH
fi
if [ "$COMPUTERNAME" == "urgi" ]; then
  # http://stackoverflow.com/a/4454754/597069
  export GIT_SSL_NO_VERIFY=true
  
  PATH=/home/fruitselgen/bin:/home/fruitselgen/texlive/bin/x86_64-linux:$PATH
  
  PYTHONPATH=/home/fruitselgen/lib/python2.7/site-packages/:$PYTHONPATH
  
  MANPATH=/home/fruitselgen/texlive/texmf-dist/doc/man:$MANPATH
  INFOPATH=/home/fruitselgen/texlive/texmf-dist/doc/info:$INFOPATH
fi
if [ "$COMPUTERNAME" == "midway" ]; then
  module load gcc
  module load gdb
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
fi

export PATH=$HOME/bin:$PATH
export PYTHONPATH=$HOME/lib/python2.7/site-packages/:$PYTHONPATH
export MANPATH=$HOME/share/man:$MANPATH
export INFOPATH=$INFOPATH
# see .Renviron for R_LIBS_USER
