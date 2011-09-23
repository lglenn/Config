HOME = ENV['HOME']

BASH_FILES = %w{bashrc bash_profile bash_logout}
ZSH_FILES = %w{zshrc java_setup}
EMACS_FILES = %w{emacs}
GIT_FILES = %w{gitignore gitconfig}
SCREEN_FILES = %w{screenrc}
VIM_FILES = %w{vimrc}

vim_dependencies = [VIM_FILES]

dotfiles = BASH_FILES + ZSH_FILES + EMACS_FILES + GIT_FILES + SCREEN_FILES + VIM_FILES

def make_dotfile_copy_task file
    target = "#{HOME}/.#{file}"
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
    make_dotfile_copy_task dotfile
end

%w{colors ftdetect plugin indent syntax}.each do |dir|
    task_symbol = "vim_#{dir}_dir".to_sym
    make_copy_dir_task "./home/vim/#{dir}/*.vim", "#{HOME}/.vim/#{dir}", task_symbol
    vim_dependencies << task_symbol
end

task :default => [:zsh, :bash, :emacs_ed, :git, :screen, :vim]

desc "zsh config"
task :zsh => ZSH_FILES

desc "bash config"
task :bash => BASH_FILES

desc "vim config"
task :vim => vim_dependencies

make_copy_dir_task './home/emacs.d/site-lisp/*.el', "#{HOME}/.emacs.d/site-lisp", :emacsdir
desc "emacs config"
task :emacs_ed => [EMACS_FILES, :emacsdir]

desc "git config"
task :git => GIT_FILES

desc "screen config"
task :screen => SCREEN_FILES

desc "vim config"
task :vim => VIM_FILES
