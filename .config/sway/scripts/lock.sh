#!/usr/bin/env bash

# swaylock --daemonize \
#          --color "$selection-color" \
#          --inside-color "$selection-color" \
#          --inside-clear-color "$text-color" \
#          --ring-color "$color2" \
#          --ring-clear-color "$color11" \
#          --ring-ver-color "$color13" \
#          --show-failed-attempts \
#          --fade-in 0.2 \
#          --grace 2 \
#          --effect-vignette 0.5:0.5 \
#          --effect-blur 7x5 \
#          --ignore-empty-password \
#          --screenshots \
#          --c

swaylock --daemonize \
	 --screenshots \
	 --indicator \
	 --indicator-radius 100 \
	 --indicator-thickness 7 \
	 --effect-blur 7x5 \
	 --effect-vignette 0.5:0.5 \
         --color "#D0BC00" \
         --inside-color "#000000" \
         --inside-clear-color "#ffffff" \
         --ring-color "#ffffff" \
         --ring-clear-color "#EF8B50" \
         --ring-ver-color "#44BC44" \
	 --grace 2 \
	 --fade-in 0.2
