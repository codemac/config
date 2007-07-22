#!/bin/zsh
#

SSH_ENV=$HOME/.ssh/environment

function start_agent {
     echo "Initialising new SSH agent..."
     /usr/bin/ssh-agent | sed 's/^echo/#echo/' >! ${SSH_ENV}
     chmod 600 ${SSH_ENV}
     . ${SSH_ENV} > /dev/null
     /usr/bin/ssh-add;
}

# Source SSH settings, if applicable

if [[ -f "${SSH_ENV}" ]]; then
     . ${SSH_ENV} > /dev/null
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent > /dev/null || {
         start_agent;
     }
else
     start_agent;
fi

# mairix database updater!
if [[ -f "$HOME/.mairix_update.lock" ]]; then
	echo "1" > /dev/null
else
	rm $HOME/.mairix_update.pid
	$HOME/hacks/mairix_update.py > /dev/null &
	PID=`pidof -x -o %PPID 'mairix_update.py'`
	echo $PID > $HOME/.mairix_update.pid
fi
