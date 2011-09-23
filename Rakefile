HOME = ENV['HOME']
BASH_FILES = %w{.bashrc .bash_profile .bash_logout}
ZSH_FILES = %w{.zshrc .java_setup}
EMACS_FILES = %w{.emacs}
GIT_FILES = %w{.gitignore .gitconfig}
SCREEN_FILES = %w{.screenrc}
VIM_FILES = %w{.vimrc}

dotfiles = BASH_FILES + ZSH_FILES + EMACS_FILES + GIT_FILES + SCREEN_FILES + VIM_FILES

def make_file_copy_task file
    target = "#{HOME}/#{file}"
    src = "./home/#{file}"
    task file => target
    file target => src do
        cp src, target
    end
end

def make_copy_dir_task src_list, target_dir, task_symbol
    mkdir_p target_dir,:verbose => false
    FileList[src_list].each do |f|
        target = "#{target_dir}/#{File.basename(f)}"
        file target => [f] do |t|
            cp f, target
        end 
        task task_symbol => target 
    end
end

dotfiles.each do |dotfile|
    make_file_copy_task dotfile
end

task :default => [:zsh, :bash, :emacs, :git, :screen, :vim]

desc "zsh config"
task :zsh => ZSH_FILES

desc "bash config"
task :bash => BASH_FILES

make_copy_dir_task './home/.vim/colors/*.vim', "#{HOME}/.vim/colors", :vimdir
desc "vim config"
task :vim => [VIM_FILES, :vimdir]

make_copy_dir_task './home/.emacs.d/site-lisp/*.el', "#{HOME}/.emacs.d/site-lisp", :emacsdir
desc "emacs config"
task :emacs => [EMACS_FILES, :emacsdir]

desc "git config"
task :git => GIT_FILES

desc "screen config"
task :screen => SCREEN_FILES

desc "vim config"
task :vim => VIM_FILES
