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
ghc -O3 -threaded SSHChan.hs Config.hs -o sshchan
ghc -O3 SSHChan.hs Config.hs -o admin
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
sudo chown "$user":anons "$dir/$name.db"
cat defaultConfig.cfg | sudo tee -a "$dir/chan.cfg"
sudo su -c "sed -i '1s/.*/chanName=\"$name\",/' ~/chan.cfg" "$user"
printf "\nMatch User $user\n    ForceCommand ./sshchan\n    PasswordAuthentication yes\n    AllowTcpForwarding no\n" | sudo tee -a /etc/ssh/sshd_config
printf "\nAlmost done! You need to restart ssh. This is done differently on different distros (http://www.cyberciti.biz/faq/howto-restart-ssh/).\n"
printf "After that's done, you've successfully installed sshchan-functional! Be sure to report any bugs!\n"

