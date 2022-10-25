#!/usr/bin/env bash

swaylock --daemonize \
         --screenshots \
         --clock \
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
         # --ring-ver-color "#44BC44" \ # Enabling this will make
         # swaylock crash on each key press
	 --grace 2 \
	 --fade-in 0.2
