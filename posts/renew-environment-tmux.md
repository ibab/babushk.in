---
title: Renew environment variables in tmux
date: 2014-10-10
---

When working with terminal multiplexers like tmux, one really annoying problem is the fact that environment variables like `SSH_AUTH_SOCK` and `DISPLAY` get outdated when reattaching to a session.
This means that you can't connect to the ssh-agent (in the case of `SSH_AUTH_SOCK`) or use X forwarding (in the case of `DISPLAY`) unless you open up a new pane.

This can interrupt your workflow greatly when using tools like git, which depend on the availability of an ssh-agent, or when trying to access the X clipboard on the remote server.

You might know that updated environment variables can be passed to tmux through the `update-environment` setting like this (in `tmux.conf`):

    set-option -g update-environment "SSH_AUTH_SOCK \
                                      SSH_CONNECTION \
                                      DISPLAY"

This means that the environment variables will be updated to the correct value when attaching, but only for tmux itself, not for the shells that are running in each pane!

It looks like some way to pass the fresh variables to each opened shell is needed.
A lot of people recommend to link the currently active ssh-agent socket to a fixed location from `.bashrc` and to set the `SSH_AUTH_SOCK` variable to that location.
This might solve the issue in the case of ssh, but doesn't work for X forwarding.

The way I'm solving the problem right now is by updating the two variables each time a new command is executed.
This has the benefit that I never have to think about updating variables manually.

In my `.zshrc`, I define a `refresh` function which gets the fresh environment variables from tmux and exports them:

```zsh
if [ -n "$TMUX" ]; then                                                                               
  function refresh {                                                                                
    export $(tmux show-environment | grep "^SSH_AUTH_SOCK")                                       
    export $(tmux show-environment | grep "^DISPLAY")                                             
  }                                                                                                 
else                                                                                                  
  function refresh { }                                                                              
fi
```

Note that the function only does something if the current shell is running inside a tmux session.

Then, I define a `preexec` hook that calls `refresh` before each new command that gets executed:

```zsh
function preexec {                                                                                    
    refresh                                                                                           
}
```

The same configuration should work for bash.

After setting up tmux and zsh this way, the variables get updated immediately and you never notice that they've gone stale.

Check out the rest of my configuration on [Github](https://github.com/ibab/dotfiles/).

