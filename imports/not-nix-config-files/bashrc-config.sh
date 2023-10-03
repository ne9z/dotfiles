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

nix-shell () { nix shell $(for i in ${@}; do printf 'nixpkgs/%s#%s ' $(nixos-version --revision) $i; done;); }

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

wfrnativeres () {
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
    # 
    # see this link for more ffmpeg video encoding options
    # https://ffmpeg.org/ffmpeg-codecs.html#VAAPI-encoders
}

wfr1080p () {
    local filename
    filename="${HOME}/Downloads/$(date +%Y%m%d_%H%M%S).mp4"
    doas /usr/bin/env sh <<EOF
        umask ugo=rw &&
	 $(command -v ffmpeg) -device /dev/dri/card0 \
         -framerate 60 \
	 -f kmsgrab \
	 -i - \
         -vf 'hwmap=derive_device=vaapi,scale_vaapi=format=nv12:w=1920:h=1080'    \
	 -c:v h264_vaapi \
	 -qp 24 "${filename}"
EOF
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
    if ! test -f "${HOME}"/.config/tubpass; then
	pass show de/tub | head -n1 > "${HOME}"/.config/tubpass
    fi
    wl-copy < "${HOME}"/.config/tubpass
}


mbootstrapdir="
/oldroot${HOME}
/oldroot${HOME}/Downloads:${HOME}/Downloads
/oldroot${HOME}/Documents:${HOME}/Documents
/oldroot${HOME}/gnus:${HOME}/Mail
/oldroot${HOME}/.gnupg:${HOME}/.gnupg
/oldroot${HOME}/.ssh/
"

mbootstrap () {
    local choice
    echo "you need to run this in a SUBSHELL. type YES if you know"
    echo "gpg.tar.xz must be already in /home/yc"
    read -r choice
    if [ "${choice}" != "YES" ]; then
	return 1
    fi

    set -ex
    local source=""
    for mount in ${mbootstrapdir}; do
	source="${mount%:*} ${source}"
    done
    doas /usr/bin/env source="${source}" user="$(whoami)" bash <<-'EOF'
set -ex
for i in ${source}; do
    if ! test -d "${i}"; then
     mkdir -p "${i}"
   fi
   chown -R  ${user}:users /oldroot/home
set -ex
done
EOF
    echo "restore gnupg"
    tar -axC /oldroot"${HOME}" -f "${HOME}"/gpg.tar.xz
    mv /oldroot/home/yc/oldroot/home/yc/.gnupg/ /oldroot/home/yc/
    ln -s /oldroot"${HOME}"/.gnupg "${HOME}"/.gnupg
    # restart gpg ssh agent
    systemctl stop --user gpg-agent-ssh.socket
    systemctl stop --user gpg-agent.service
    systemctl start --user gpg-agent-ssh.socket
    echo "clone password repo"
    git clone tl.yc:~/githost/pass /oldroot"${HOME}"/.password-store
    echo "clone sysconf repo"
    git clone tl.yc:~/githost/systemConfiguration /oldroot"${HOME}"/nixos-config
    mkdir -p "${HOME}"/Maildir/posteo
    echo "RETURN_SUCCESS"
    set +ex
}

conedu () {
    nmcli connection add \
	  type wifi \
	  connection.id eduroam \
	  wifi.mode infrastructure \
	  wifi.ssid eduroam \
	  wifi-sec.auth-alg open \
	  wifi-sec.key-mgmt wpa-eap \
	  802-1x.eap peap \
	  802-1x.identity yguo@tu-berlin.de \
	  802-1x.password "$(cat "${HOME}"/.config/tubpass)" \
	  802-1x.phase2-auth mschapv2
    nmcli connection up eduroam
}
