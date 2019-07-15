#!/usr/bin/env bash

# soure: https://github.com/DominikMueller64/install_R_source/blob/master/install_R_source.sh

# exit on error
set -e

if [ -z "$*" ]; then echo "Missing R version (e.g., 3.4.1)"; exit 1; fi

R_VERSION=$1

echo "Create temporary directory ~/R_tmp..."
mkdir -p $HOME/R_tmp
cd $HOME/R_tmp

echo "Download and extract R source..."
wget https://cran.r-project.org/src/base/R-3/R-${R_VERSION}.tar.gz
tar xvzf R-${R_VERSION}.tar.gz
rm R-${R_VERSION}.tar.gz

echo "Configure and make..."
cd R-${R_VERSION}
./configure --prefix=/usr/local/R/${R_VERSION} --enable-R-shlib
make
sudo make install

echo "Cleanup..."
cd ../..
rm -rf $HOME/R_tmp

echo "Create symbolic links..."
sudo ln -s /usr/local/R/${R_VERSION}/bin/R /usr/bin/R-${R_VERSION}
sudo ln -s /usr/local/R/${R_VERSION}/bin/Rscript /usr/bin/Rscript-${R_VERSION}

echo The R-${R_VERSION} executable, and Rscript, are now available in /usr/bin/R-${R_VERSION} and /usr/bin/Rscript-${R_VERSION}.
