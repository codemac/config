#!/bin/zsh

# Delete left overs from running Xorg
if [[ -f ~/.serverauth.* ]]; then
	rm -f ~/.serverauth.*
fi

# Invalidate sudo session
sudo -k
