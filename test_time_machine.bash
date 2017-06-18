#!/usr/bin/env bash

rm -r fake_home; mkdir -p fake_home
echo "a" > fake_home/dat1
echo "a" > fake_home/dat2

rm -r backup; mkdir -p backup
mkdir -p backup/bkp_latest

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/backup -p bkp_latest -c bkp1 -e ""

# no change to dat1
echo "b" >> fake_home/dat2

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/backup -p bkp_latest -c bkp2 -e ""

# no change to dat1
echo "c" >> fake_home/dat2
echo "a" > fake_home/dat3

~/src/perso/time_machine -s ~/tmp/test_rsync_TM/fake_home -t ~/tmp/test_rsync_TM/backup -p bkp_latest -c bkp3 -e ""

du -hs backup/*

rm -r backup/bkp1
rm -r backup/bkp2



~/src/perso/time_machine -h
nohup ~/src/perso/time_machine >& time_machine.log &
