
HOME = '/Users/lglenn2'
BASH_FILES = %w{.bashrc .bash_profile .bash_logout}
ZSH_FILES = %w{.zshrc .java_setup}
EMACS_FILES = %w{.emacs}
GIT_FILES = %w{.gitignore .gitconfig}
SCREEN_FILES = %w{.screenrc}
VIM_FILES = %w{.vimrc}

dotfiles = BASH_FILES + ZSH_FILES + EMACS_FILES + GIT_FILES + SCREEN_FILES + VIM_FILES

dotfiles.each do |dotfile|
  target = "#{HOME}/#{dotfile}"
  src = "./home/#{dotfile}"
  task dotfile => target
  file target => src do
    sh "cp #{src} #{target}"
  end
end

task :default => [:zsh, :bash, :emacs, :git, :screen, :vim]

desc "zsh config"
task :zsh => ZSH_FILES

desc "bash config"
task :bash => BASH_FILES

desc "vim config"
task :vim => VIM_FILES do
  mkdir_p "#{HOME}/.vim/colors"
  FileList['./home/.vim/colors/*.vim'].each do |file|
    sh "cp #{file} #{HOME}/.vim/colors"
  end
end

desc "emacs config"
task :emacs => EMACS_FILES do
  mkdir_p "#{HOME}/.emacs.d//site-lisp"
  FileList['./home/.emacs.d/site-lisp/*el'].each do |file|
    sh "cp #{file} #{HOME}/.emacs.d/site-lisp/"
  end
end

desc "git config"
task :git => GIT_FILES

desc "screen config"
task :screen => SCREEN_FILES

desc "vim config"
task :vim => VIM_FILES
