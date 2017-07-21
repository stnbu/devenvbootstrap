#!/bin/sh

~/git && cd ~/git/
git -c http.sslVerify=false clone https://github.com/stnbu/devenvbootstrap.git
cd devenvbootstrap/
python3 do_emacs.py
. ~/.bashrc
emacs --daemon
