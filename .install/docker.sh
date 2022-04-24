#!/usr/bin/env bash

if dpkg -s docker-ce; then
    exit 0;
fi

curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

sudo apt-key fingerprint 0EBFCD88

sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(. /etc/os-release; echo "$UBUNTU_CODENAME") \
   stable"

sudo apt update

sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose

sudo usermod -aG docker ${USER}

sudo curl -L https://github.com/docker/compose/releases/download/1.25.5/docker-compose-`uname -s`-`uname -m` -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

docker --version
docker-compose --version

