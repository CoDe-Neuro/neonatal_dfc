#!/usr/bin/with-contenv bash

# From http://www.derekmpowell.com/posts/2018/02/docker-tutorial-3/

GIT_USER=${GIT_USER:=none}
GIT_EMAIL=${GIT_EMAIL:=none}

if [ "$GIT_USER" != none ]; then
	echo -e "[user]\n\tname=$GIT_USER\n\temail=$GIT_EMAIL" > /home/$USER/.gitconfig
fi
