#!/usr/bin/env bash

# Aim: install a specific R version so that several coexist
# Copyright (C) 2019 Timothée Flutre
# License: GPL-3+
# Person: Timothée Flutre [cre,aut]
# Versioning: https://github.com/timflutre/perso
# Inspiration: https://github.com/DominikMueller64/install_R_source/blob/master/install_R_source.sh

# To compile R with Cairo support:
# sudo apt-get install libcairo2-dev libgtk2.0-dev
# R> capabilities()["cairo"]

# exit on error
set -e

if [ -z "$*" ]; then echo "Missing R version (e.g., 3.5.3)"; exit 1; fi

R_VERSION=$1

if [ -f /usr/bin/R-${R_VERSION} -a -f /usr/bin/Rscript-${R_VERSION} ]; then
  
  echo The R and Rscript for version ${R_VERSION} are already available, as /usr/bin/R-${R_VERSION} and /usr/bin/Rscript-${R_VERSION}.
  
else
  
  echo "Create temporary directory ~/R_tmp..."
  mkdir -p $HOME/R_tmp
  cd $HOME/R_tmp
  
  if [ ! -f R-${R_VERSION}.tar.gz ]; then 
    echo "Download and extract R source..."
    R_VERSION_MAIN=$(echo $R_VERSION | awk '{split($0,a,"."); printf a[1]}')
    wget https://cran.r-project.org/src/base/R-${R_VERSION_MAIN}/R-${R_VERSION}.tar.gz
    tar xvzf R-${R_VERSION}.tar.gz
    # rm -f R-${R_VERSION}.tar.gz
  fi
  
  echo "Configure and make..."
  cd R-${R_VERSION}
  ./configure --prefix=/usr/local/R/${R_VERSION} --enable-R-shlib --with-cairo
  make
  sudo make install
  
  echo "Create symbolic links..."
  sudo ln -sf /usr/local/R/${R_VERSION}/bin/R /usr/bin/R-${R_VERSION}
  sudo ln -sf /usr/local/R/${R_VERSION}/bin/Rscript /usr/bin/Rscript-${R_VERSION}
  
  echo "Cleanup..."
  cd ../..
  rm -rf $HOME/R_tmp
  
  echo The R-${R_VERSION} executable, and Rscript, are now available as /usr/bin/R-${R_VERSION} and /usr/bin/Rscript-${R_VERSION}.
  
fi

# Local Variables:
# mode: Shell-script
# coding: utf-8-unix
# tab-width: 2
# sh-basic-offset: 2
# sh-indentation: 2
# End:
