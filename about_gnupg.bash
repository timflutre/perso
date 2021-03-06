#!/usr/bin/env bash

# https://gnupg.org/
# https://help.ubuntu.com/community/GnuPrivacyGuardHowto
# http://parabing.com/2014/11/gnupg1of2/
# http://parabing.com/2014/12/gnupg2of2/
# http://orgmode.org/worg/org-tutorials/encrypting-files.html

INSTALL_PREFIX="/usr/local"
DWNLOAD_PREFIX="${HOME}/src_ext"
ASSUANV="2.2.1"
KSBAV="1.3.3"
GNUPGV="2.0.28"

mkdir -p ${DWNLOAD_PREFIX}

# https://www.gnupg.org/related_software/libassuan/index.en.html
cd ${DWNLOAD_PREFIX}
mkdir -p assuan; cd assuan/
wget ftp://ftp.gnupg.org/gcrypt/libassuan/libassuan-${ASSUANV}.tar.bz2
wget ftp://ftp.gnupg.org/gcrypt/libassuan/libassuan-${ASSUANV}.tar.bz2.sig
sha1sum libassuan-${ASSUANV}.tar.bz2
echo -e "Is the SHA-1 sum above the same as the one on\nhttps://www.gnupg.org/download/integrity_check.en.html? [y/n]"
read -s IsSameSha1SumAssuan
if [ "${IsSameSha1SumAssuan}" != "y" ]; then exit 0; fi
tar -xvf libassuan-${ASSUANV}.tar.bz2
cd libassuan-${ASSUANV}/
./configure --prefix=${INSTALL_PREFIX}
make
make check
sudo make install
make clean

# https://www.gnupg.org/related_software/libksba/index.html
cd ${DWNLOAD_PREFIX}
mkdir -p ksba; cd ksba/
wget ftp://ftp.gnupg.org/gcrypt/libksba/libksba-${KSBAV}.tar.bz2
wget ftp://ftp.gnupg.org/gcrypt/libksba/libksba-${KSBAV}.tar.bz2.sig
sha1sum libksba-${KSBAV}.tar.bz2
echo -e "Is the SHA-1 sum above the same as the one on\nhttps://www.gnupg.org/download/integrity_check.en.html? [y/n]"
read -s IsSameSha1SumKsba
if [ "${IsSameSha1SumKsba}" != "y" ]; then exit 0; fi
tar -xvf libksba-${KSBAV}.tar.bz2
cd libksba-${KSBAV}/
./configure --prefix=${INSTALL_PREFIX}
make
make check
sudo make install
make clean

# https://www.gnupg.org/download/index.html
cd ${DWNLOAD_PREFIX}
mkdir -p gnupg; cd gnupg/
wget ftp://ftp.gnupg.org/gcrypt/gnupg/gnupg-${GNUPGV}.tar.bz2
wget ftp://ftp.gnupg.org/gcrypt/gnupg/gnupg-${GNUPGV}.tar.bz2.sig
sha1sum gnupg-${GNUPGV}.tar.bz2
echo -e "Is the SHA-1 sum above the same as the one on\nhttps://www.gnupg.org/download/integrity_check.en.html? [y/n]"
read -s IsSameSha1SumGnupg
if [ "${IsSameSha1SumGnupg}" != "y" ]; then exit 0; fi
tar -xvf gnupg-${GNUPGV}.tar.bz2
cd gnupg-${GNUPGV}/
./configure --sysconfdir=/etc --localstatedir=/var --prefix=${INSTALL_PREFIX}
make
make check
sudo make install
make clean

cd
which gpg2
gpg2 --version

# http://bugs.gnupg.org/gnupg/issue1656
# https://wiki.gnupg.org/GnomeKeyring

# Generate a key (public and private):
# gpg2 --list-keys
# gpg2 --gen-key
# gpg2 --list-keys
# export GPGKEY=D8FC66D2 # example

# Create a revocation certificate:
# gpg2 --armor --output revcert_compromised-key.asc --gen-revoke $GPGKEY
# chmod go=-r-w-x revcert_compromised-key.asc
# copy this file to a USB key stored in a physically secure location

# Backing up your key:
# gpg2 --list-keys
# gpg2 -ao <...>-public.key --export $GPGKEY
# chmod go=-r-w-x <...>-public.key
# gpg2 --list-secret-keys
# gpg2 -ao <...>-private.key --export-secret-keys $GPGKEY
# chmod go=-r-w-x <...>-private.key
# copy these files to a USB key stored in a physically secure location

# Import keys:
# gpg2 --import <...>-public.key
# gpg2 --import <...>-private.key

# Set their trust level:
# gpg2 --edit-key my@email.com
# > trust ; 5 ; y ; quit

# gpg2 --send-keys $GPGKEY
# gpg2 --search-key "you@email.net"

# Example of encrypting/decrypting a file:
# gpg2 --armor --output file.txt.asc --recipient you@email.net --encrypt file.txt
# gpg2 --output file_decrypt.txt --decrypt file.txt.asc

# http://security.stackexchange.com/questions/51771/where-do-you-store-your-personal-private-gpg-key
# http://security.stackexchange.com/questions/68187/is-it-necessary-to-remove-the-primary-secret-gpg-key-from-your-laptop-if-it-has
# https://wiki.debian.org/Subkeys?action=show&redirect=subkeys
