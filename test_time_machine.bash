#!/usr/bin/env bash

cd ~/tmp
rm -rf test_rsync_TM; mkdir -p test_rsync_TM
cd test_rsync_TM

rm -rf fake_home; mkdir -p fake_home
echo "a" > fake_home/dat1
echo "a" > fake_home/dat2

rm -rf fake_backup; mkdir -p fake_backup
mkdir -p fake_backup/bkp_latest

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/fake_backup -p bkp_latest -c bkp1 -e ""

# no change to dat1
echo "b" >> fake_home/dat2

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/fake_backup -p bkp_latest -c bkp2 -e ""

# no change to dat1
echo "c" >> fake_home/dat2
echo "a" > fake_home/dat3

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/fake_backup -p bkp_latest -c bkp3 -e ""

du -hs fake_backup/*

rm -r fake_backup/bkp1
rm -r fake_backup/bkp2

# test with .git
mkdir fake_home/mypkg
echo "a" > fake_home/mypkg/code.sh
mkdir fake_home/mypkg/.git
echo "a" > fake_home/mypkg/.git/config

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/fake_backup -p bkp_latest -c bkp4 -e ""



~/src/perso/time_machine -h
nohup ~/src/perso/time_machine >& time_machine.log &
