#!/bin/bash

printf "Hi, and welcome to the interactive sshchan-functional setup script!\n"
printf "If you've already installed sshchan before, this script would probably mess everything up, so probably just don't run this if you have.\n"
printf "BEFORE you start this setup script, you should already have openssh-server installed, so if you don't, Ctrl+C now!"
read 
printf "Got it? Good!\n"
printf "First we'll compile sshchan (note: you need ghc and cabal to do this! If you don't have them, get them!)\n"
cabal install mtl
cabal install text
cabal install brick
cabal install sqlite-simple
ghc -O3 -threaded sshchan.hs
ghc -O3 admin.hs
printf "Done! Enter your chan's name: "
read name
cat setup.sql | sqlite3 "$name.db"
printf "Enter the name of the user your sshchan-functional instance should run on: "
read user
printf "And what directory should sshchan be installed to?: "
read dir
groupadd anons
useradd -d "$dir" -m -g anons -N -c 'sshchan user' "$user"
printf "Now we'll set the password that will be used to connect to your sshchan instance.\n"
passwd "$user"
printf "Match User $user\n    ForceCommand ./sshchan"
mv sshchan "$dir"
mv admin "$dir"
mv "$name.db" "$dir"
echo "$name" > "$dir/name.txt"
printf "That should be it! Enjoy sshchan-functional!\n"

