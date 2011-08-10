
HOME = '/Users/lglenn2'
BASH_FILES = %w{.bashrc .bash_profile .bash_logout}
ZSH_FILES = %w{.zshrc}
EMACS_FILES = %w{.emacs}
GIT_FILES = %w{.gitignore .gitconfig}
SCREEN_FILES = %w{.screenrc}

dotfiles = BASH_FILES + ZSH_FILES + EMACS_FILES + GIT_FILES + SCREEN_FILES

dotfiles.each do |dotfile|
  target = "#{HOME}/#{dotfile}"
  src = "./HOME/#{dotfile}"
  task dotfile => target
  file target => src do
    sh "cp #{src} #{target}"
  end
end

task :default => [:zsh, :bash, :emacs, :git, :screen]

desc "zsh config"
task :zsh => ZSH_FILES

desc "bash config"
task :bash => BASH_FILES

desc "emacs config"
task :emacs => EMACS_FILES

desc "git config"
task :git => GIT_FILES

desc "screen config"
task :screen => SCREEN_FILES
