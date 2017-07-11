
PATH=~/.herp/bin:"$PATH"
alias ec=emacsclient

if [ -f /etc/profile.d/bash_completion.sh ] ; then
   . /etc/profile.d/bash_completion.sh
fi

if [ -f /etc/bash_completion ] ; then
   . /etc/bash_completion
fi

export LESS='-ir'
