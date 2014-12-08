HOME = ENV['HOME']

BASH_FILES = { "home/bash" => %w{bashrc bash_profile bash_logout} }
ZSH_FILES = { "home/zsh" => %w{zshrc java_setup} }
GIT_FILES = { "home/git" => %w{gitignore gitconfig} }
SCREEN_FILES = { "home/screen" => %w{screenrc} }
TMUX_FILES = { "home/tmux" => %w{tmux.conf} }
EMACS_FILES = { "home/emacs" => %w{emacs} }

def make_dotfile_copy_task src_dir,file
    target = "#{HOME}/.#{file}"
    src = "#{src_dir}/#{file}"
    file target => src do
        cp src, target
    end
    task file => target
end

def make_copy_dir_task src_list, target_dir, task_symbol
    tasks = []
    mkdir_p target_dir,:verbose => false
    FileList[src_list].each do |f|
        target = "#{target_dir}/#{File.basename(f)}"
        file target => [f] do |t|
            cp f, target
        end 
        tasks << task(task_symbol => target) 
    end
    return tasks
end

def makedeps files
    deps = []
    files.each do |src_dir,files|
        files.each do |file|
            deps << make_dotfile_copy_task(src_dir, file)
        end
    end
    return deps
end

zsh_deps = makedeps ZSH_FILES
bash_deps = makedeps BASH_FILES
git_deps = makedeps GIT_FILES
screen_deps = makedeps SCREEN_FILES
tmux_deps = makedeps TMUX_FILES

ipython_deps = []
%w{profile_default}.each do |dir|
    task_symbol = "ipython_#{dir}_dir".to_sym
    make_copy_dir_task "./home/ipython/#{dir}/*", "#{HOME}/.ipython/#{dir}", task_symbol
    ipython_deps << task_symbol
end

emacs_deps = makedeps EMACS_FILES
emacs_deps += make_copy_dir_task('./home/emacs/emacs.d/site-lisp/*.el', "#{HOME}/.emacs.d/site-lisp", :emacsdir)

task :default => [:zsh, :bash, :gnu_emacs, :git, :screen, :tmux, :ipython]

desc "zsh config"
task :zsh => zsh_deps

desc "bash config"
task :bash => bash_deps

desc "emacs config"
task :gnu_emacs => emacs_deps

desc "git config"
task :git => git_deps

desc "screen config"
task :screen => screen_deps

directory "#{ENV['HOME']}/.tmux/plugins"
file "#{ENV['HOME']}/.tmux/plugins" do
  cp_r Dir["home/tmux/tmux/plugins"], "#{ENV['HOME']}/.tmux"
end
task :tmux => "#{ENV['HOME']}/.tmux/plugins"

desc "tmux config"
task :tmux => tmux_deps

desc "ipython config"
task :ipython => ipython_deps
