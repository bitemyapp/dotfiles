#!/usr/bin/env bash

# Add Docker's official GPG key:
sudo apt-get update
sudo apt-get install ca-certificates curl
sudo install -m 0755 -d /etc/apt/keyrings
sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
sudo chmod a+r /etc/apt/keyrings/docker.asc

# Add the repository to Apt sources:
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt-get update

sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

target_user="${SUDO_USER:-$USER}"

if getent group docker >/dev/null; then
  echo "docker group already exists."
else
  echo "group does not exist, creating it"
  sudo groupadd docker
fi

sudo usermod -aG docker "$target_user"

if id -nG "$target_user" | tr ' ' '\n' | grep -qx docker; then
  echo "$target_user is in the docker group."
else
  echo "$target_user was added to the docker group."
fi

if id -nG | tr ' ' '\n' | grep -qx docker; then
  docker run hello-world
else
  echo "Current shell does not have the docker group yet; log out and back in before running docker without sudo."
  sudo -u "$target_user" -g docker docker run hello-world
fi
