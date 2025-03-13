#!/bin/bash

pgrep -q Emacs || exit 1

# Get current appearance
appearance=$(defaults read -g AppleInterfaceStyle &>/dev/null && echo "dark" || echo "light")

case "$1" in
    "on")    expr="(progn (setq my/redshift? t) (my/apply-theme '$appearance))" ;;
    "off")   expr="(progn (setq my/redshift? nil) (my/apply-theme '$appearance))" ;;
    "toggle") expr="(progn (setq my/redshift? (not my/redshift?)) (my/apply-theme '$appearance))" ;;
    *) 
        echo "Usage: $0 [on|off|toggle]"
        exit 1
        ;;
esac

/opt/homebrew/bin/emacsclient -e "$expr"
