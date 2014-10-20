# User specific aliases and functions
alias ls='ls -F'
alias ll='ls -l'

# need to set PS1 here so that emacs term-mode can have it
# https://www.digitalocean.com/community/tutorials/how-to-customize-your-bash-prompt-on-a-linux-vps
PS1="\u@\h:\w\$ "

# CarthaGene local install
export PATH=/home/flutre/src_ext/carthagene/bin:$PATH
