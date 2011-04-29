# Set emacs key bindings in line editor
bindkey -e

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
sshhosts=(goop.metacism.net sd1pbk01lx.saksdirect.com sd1pzp01lx.saksdirect.com sd1pzp02lx.saksdirect.com sd1pzp03lx.saksdirect.com sd1pzp04lx.saksdirect.com sd1pzp05lx.saksdirect.com sd1pzp06lx.saksdirect.com sd1pzp07lx.saksdirect.com sd1pzp08lx.saksdirect.com);

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

# OS-Specific functionality.
case `uname` in
    Darwin)
	;;
    Linux)
	;;
esac
