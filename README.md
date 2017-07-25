# devenvbootstrap

TODO
----

* automatic virtualenv smartness
* the forever-appened shell history
* ispell + dictionary  (aspell-en)
* distro agnosticism
* install tmux, bash completion, epel(wat)
* bootstrap from py2?

MISC
----

RPM Annoyances
==============

* rsync -xa 10.20.64.166:/etc/yum.repos.d/ /etc/yum.repos.d/
* wget https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
* sudo rpm -Uvh epel-release-latest-7*.rpm

Notes for Me
============

* git -c http.sslVerify=false clone
* from remote_pdb import RemotePdb ; RemotePdb('0.0.0.0', 4444).set_trace()
* wget -O - http://tiny.cc/sdevenv | python3 -
