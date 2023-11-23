#!/usr/bin/env bash
# shellcheck disable=SC2292,SC2312

# TIP: check with ShellCheck
# Always use null-terminated variants of commands, such as
# git ls-files -z
# grep -z
# xargs --verbose -0
# find -print0
# REASON:
# POSIX allow any character other than slash / and null-character \0
# to be included in a file name.  Therefore, when processing a list of
# file names, the list of names is only guaranteed to
# be distinguished from one another when null-character is used to separate them

y () {
    mpv "${@}"
}

nmt () {
    git ls-files -z | grep -z '\.nix$' | xargs --verbose -0I'{}' nixfmt '{}'
}

Ns () {
    doas nixos-rebuild switch --flake "git+file://${HOME}/nixos-config"
}

Nb () {
    doas nixos-rebuild boot --flake "git+file://${HOME}/nixos-config"
}

doa ()
{
    doas -s
}

gpub ()
{
    local input=${1}
    local git_paths="$HOME/nixos-config $HOME/Downloads/tub $HOME/.password-store"
    if [ "${input}" == "s" ]; then
        for path in ${git_paths}; do
            echo "${path}"
            git -C "${path}" status
            git -C "${path}" push
            echo "================"
        done
    else
        for path in ${git_paths}; do
            echo "${path}"
            git -C "${path}" status
            git -C "${path}" pull --rebase
            echo "================"
        done
    fi
}

lowres () {
    echo "Entering 720p efficient presentation mode"
    swaymsg -- input "9580:110:PenTablet_Pen" map_to_region 0 0 1280 720
    swaymsg -- 'output * scale 1; output * mode --custom 1280x720@60Hz'
    echo "Restore to native resolution by reloading Sway config"
    echo "Super Shift c"
}


wfr () {
    lowres
    local filename
    filename="${HOME}/Downloads/$(date +%Y%m%d_%H%M%S).mp4"
    doas /usr/bin/env sh <<EOF
        umask ugo=rw &&
	 $(command -v ffmpeg) -device /dev/dri/card0 \
         -framerate 60 \
	 -f kmsgrab \
	 -i - \
         -vf 'hwmap=derive_device=vaapi,scale_vaapi=format=nv12'    \
	 -c:v h264_vaapi \
	 -qp 24 "${filename}"
EOF
    echo "Restore to native resolution by reloading Sway config"
    echo "Super Shift c"
    #
    # see this link for more ffmpeg video encoding options
    # https://ffmpeg.org/ffmpeg-codecs.html#VAAPI-encoders
}


gm () {
    printf "laptop brightness: b\n"
    printf "gammastep:         g\n"
    printf "inverse color:     i\n"
    printf "laptop screen:     s\n"
    local choice
    read -r choice
    case "${choice}" in
	b)
	    printf "set minimum: m\n"
	    printf "set percent: p PERCENT\n"
	    local percent
	    read -r choice percent
	    case "${choice}" in
		m)
		    brightnessctl set 3%
		    ;;
		p)
		    brightnessctl set "${percent}"%
		    ;;
                *)
                    printf "no input given"
                    return 1
                    ;;
	    esac
	    ;;
	g)
	    printf "monitor dim day:   md\n"
	    printf "monitor dim night: mn\n"
	    printf "laptop  dim night: ld\n"
	    printf "reset:   r\n"
	    read -r choice
	    case "${choice}" in
		md)
		    (gammastep -O 5000 -b 0.75 &)
		    ;;
		mn)
		    (gammastep -O 3000 -b 0.56 &)
		    ;;
		ld)
		    (gammastep -O 3000 &)
		    ;;
		r)
		    pkill gammastep
		    (gammastep -x &)
		    pkill gammastep
		    ;;
                *)
                    printf "no input given"
                    return 1
                    ;;
	    esac
	    ;;
	i)
            printf "invert color and dim: m\n"
            printf "invert color:      i\n"
	    printf "reset invert:      r\n"
	    read -r choice
	    case "${choice}" in
                m)
                    (wl-gammactl -c -1 -b 1.5 -g 1&)
                    ;;
                i)
                    (wl-gammactl -c -1 -b 2 -g 1 &)
                    ;;
		r)
                    pkill wl-gammactl
		    ;;
                *)
                    printf "no input given"
                    return 1
                    ;;
	    esac
	    ;;
	s)
	    printf "disable: d\n"
	    printf "enable:  e\n"
	    read -r choice
	    case "${choice}" in
		d)
		    swaymsg  output eDP-1 disable
		    swaymsg  output LVDS-1 disable
		    ;;
		e)
		    swaymsg  output eDP-1 enable
		    swaymsg  output LVDS-1 enable
		    ;;
                *)
                    printf "no input given"
                    return 1
                    ;;
	    esac
	    ;;
        *)
            printf "no input given"
            return 1
            ;;
    esac
}

tubb () {
    if [ -f "${HOME}"/Documents/wifipass.txt ]; then
        source "${HOME}"/Documents/wifipass.txt
        echo -n "${PASS_TU_BERLIN}" | wl-copy
    else
        echo "no pass! Put pass in "
        echo "${HOME}"/Documents/wifipass.txt
        echo 'with PASS_TU_BERLIN="YOUR_PASS"'
        return 1
    fi
}
