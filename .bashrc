# .bashrc
# (use .profile)

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific aliases and functions
if [ ! -f ~/.ssh/id_rsa ]; then   
    echo 'No public/private RSA keypair found.'
    ssh-keygen -t rsa -b 2048 -f ~/.ssh/id_rsa -N ""    
    cat ~/.ssh/id_rsa.pub > ~/.ssh/authorized_keys
    chmod 644 ~/.ssh/authorized_keys
fi

# Load saved modules
module load null

# User specific aliases and functions

