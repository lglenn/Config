export PS1="\h:\w> ";
export PS2="> ";

# RVM
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.

# Amazon EC2
export JAVA_HOME=/Library/Java/Home
export EC2_HOME=$HOME/ec2;
export PATH=$PATH:$EC2_HOME/bin;
