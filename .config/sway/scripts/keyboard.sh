#!/bin/sh

# NOTE this was preventing sway input to change the layout. When this
# runs on autostart, no other layout than the main layout is available
# for xkb.

# script that sets the locale from current locale settings
# swaymsg input type:keyboard xkb_layout "$(localectl status | grep "X11 Layout" | sed -e "s/^.*X11 Layout://")"

# if localectl status | grep "X11 Variant" ; then
#     swaymsg input type:keyboard xkb_variant "$(localectl status | grep "X11 Variant" | sed -e "s/^.*X11 Variant://")"
# fi
