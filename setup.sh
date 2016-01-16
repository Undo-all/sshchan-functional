#!/bin/bash

printf "Hi, and welcome to the interactive sshchan-functional setup script!\n"
printf "If you've already installed sshchan before, this script would probably mess everything up, so probably just don't run this if you have.\n"
printf "BEFORE you start this setup script, you should already have:\n"
printf "* openssh-server\n"
printf "* ghc\n"
printf "* cabal\n"
printf "all installed, so if you don't, Ctrl+C now and get them! [Enter to continue]"
read 
printf "Got them? Good!\n"
printf "First we'll compile sshchan.\n"
cabal install mtl text sqlite-simple brick 
ghc -O3 -threaded sshchan.hs
ghc -O3 admin.hs
printf "Done! Enter your chan's name: "
read name
printf "Enter the name of the user your sshchan-functional instance should run on: "
read user
printf "And what directory should the user be created in? (essentially acts as install directory): "
read dir
sudo groupadd anons
sudo useradd -d "$dir" -m -g anons -N -c 'sshchan user' "$user"
printf "Now we'll set the password that will be used to connect to your sshchan instance.\n"
sudo passwd "$user"
sudo mv sshchan "$dir"
sudo mv admin "$dir"
cat setup.sql | sudo sqlite3 "$dir/$name.db"
echo "$name" | sudo tee -a "$dir/name.txt"
printf "Match User $user\n    ForceCommand ./sshchan" | sudo tee -a /etc/ssh/sshd_config
printf "\nAlmost done! You need to restart ssh. This is done differently on different distros (http://www.cyberciti.biz/faq/howto-restart-ssh/).\n"
printf "After that's done, you've successfully installed sshchan-functional! Be sure to report any bugs!\n"

