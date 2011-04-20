# Uncommment this line for vi binding in line editor
#bindkey -v

stty intr ^c

namedir () { $1=$PWD ;  : ~$1 }

DIRSTACKSIZE=10
setopt autopushd pushd_ignore_dups pushdsilent pushdtohome
setopt appendhistory
setopt auto_name_dirs
setopt autocd
setopt histignoredups
setopt magic_equal_subst
setopt extended_glob
setopt correct

setopt listtypes
setopt autolist
setopt automenu
setopt GLOB_SUBST;

alias dh='dirs -v'

ftphosts=();
telnethosts=(localhost)
pinghosts=(localhost google.com)
sshhosts=(goop.metacism.net);

compctl -k cvscommands + -f cvs
compctl -j -P "%" kill disown bg fg
compctl -S "=" -E + -f export
compctl -u finger mail talk 
compctl -c which whence whereis man
compctl -k sshhosts ssh
compctl -k sshhosts scp
compctl -k ftphosts ftp
compctl -k ftphosts ncftp
compctl -k telnethosts telnet
compctl -k pinghosts ping
compctl -S "." -q -K findpackage java
compctl -S "." -q -K findpackage javap
compctl -S "." -q -K findpackage jdb
compctl -K nswhois whois

function nswhois() {
    set -A reply;
    reply=("$1@whois.networksolutions.com");
}

# Complete java invocations by converting a.b.c to a/b/c, 
# looking for completions in CLASSPATH, and then converting 
# the resulting filename back to a Java package and/or class
# name.
function findpackage() {
  # Don't complain if there's no match.
  setopt null_glob;

  # clear reply
  set -A reply;

  # convert dots to slashes to convert 
  # a package/class to a path
  set -A tmp ${(s:.:)=1};
  mydir=${(j:/:)=tmp};

  for i in ${(s&:&)=CLASSPATH} 
  do      
    if test -d $i
    then
      for j in $i/$mydir*
      do
        j=${j%.class};
	set -A tmp ${(s:/:)=${j#$i}};
	reply=($reply ${(j:.:)tmp});
      done
    fi
  done
  return;
}

chpwd() {
	[[ -t 1 ]] || return
	case $TERM in
		sun-cmd)
			print -Pn "\e]1%~\e\\"
			;;
		rxvt | kterm | xterm)
			print -Pn "\e]2;%m:%~\a"
			;;
	esac
}	

killit() {
	ps ax | grep $1 | awk '{print $1}' | xargs kill -9
}
			
append_path() {
	found="";
	for i in $path
	do
		if [ "$i" = "$1" ]
		then
			found=1;
		fi
	done
	if [ ! "$found" ]
	then
		if [ -d "$1" ]
		then 
			path=($path $1);
		fi
	fi
}

i () { 
	date "+%D %T in $*" >>$HOME/t
}

o () { 
	date "+%D %T out $*" >>$HOME/t
}

b () { 
	if [ ! "$1" ]
	then
		echo "b requires a break length parameter";
	else 
		date "+%D %T break $*" >>$HOME/t
	fi
}

#
# umask 
#
umask 002

#
# aliases
#
alias zsh='exec zsh'

export PROMPT='%(#.%B%t:%m:%~#%b.%m:%~>) '
export HISTFILE=/tmp/history
export HISTSIZE=1000

limit coredumpsize 0

# "local" to the MacBook
alias goop="ssh goop"
alias myth="ssh myth"

