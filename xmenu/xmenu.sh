#!/bin/sh

PLAYERCOUNT=`playerctl --list-all | wc -l`
PLAYERONE=`playerctl --list-all | sed -n '1p'`
PLAYERTWO=`playerctl --list-all | sed -n '2p'`
PLAYERTHREE=`playerctl --list-all | sed -n '3p'`
STATUSONE=`playerctl --player=$PLAYERONE status`
STATUSTWO=`playerctl --player=$PLAYERTWO status`
STATUSTHREE=`playerctl --player=$PLAYERTHREE status`
FORMATONE=`playerctl metadata --player=$PLAYERONE --format '{{artist}} - {{title}}'`
FORMATTWO=`playerctl metadata --player=$PLAYERTWO --format '{{artist}} - {{title}}'`
FORMATTHREE=`playerctl metadata --player=$PLAYERTHREE --format '{{artist}} - {{title}}'`

if [[ $STATUSONE == "Playing" ]]; then 
	CHANGESTATEONE="Pause"; 
elif [[ $STATUSONE == "Paused" ]]; then  
	CHANGESTATEONE="Play"; 
 fi

if [[ $STATUSTWO == "Playing" ]]; then 
	CHANGESTATETWO="Pause"; 
elif [[ $STATUSTWO == "Paused" ]]; then  
	CHANGESTATETWO="Play"; 
 fi

if [[ $STATUSTHREE == "Playing" ]]; then 
	CHANGESTATETHREE="Pause"; 
elif [[ $STATUSTHREE == "Paused" ]]; then  
	CHANGESTATETHREE="Play"; 
 fi

xmenu <<EOF | sh &
Applications
	IMG:$HOME/xmenu/icons/web.png		Web Browser	firefox
	IMG:$HOME/xmenu/icons/gimp.png	Image editor	gimp

Terminal	alacritty

Powermenu
	IMG:$HOME/xmenu/icons/Shutdown.png	Shutdown	systemctl poweroff
	IMG:$HOME/xmenu/icons/Reboot.png		Reboot		systemctl reboot
	IMG:$HOME/xmenu/icons/Lock.png		Lock		$HOME/i3lock/lock.sh
	IMG:$HOME/xmenu/icons/Sleep.png		Sleep 		systemctl suspend
	IMG:$HOME/xmenu/icons/Exit.png		Exit	    if [[ "$DESKTOP_SESSION" == "Openbox" ]]; then openbox --exit; elif [[ "$DESKTOP_SESSION" == "bspwm" ]]; then bspc quit; elif [[ "$DESKTOP_SESSION" == "i3" ]]; then i3-msg exit; elif [[ "$DESKTOP_SESSION" == "xmonad" ]]; then $HOME.xmonad/xmonadctl 39;	fi

Players ($PLAYERCOUNT) 
	$PLAYERONE	
		$FORMATONE
			IMG:$HOME/xmenu/icons/Previous.png			Previous			playerctl --player "$PLAYERONE" previous
			IMG:$HOME/xmenu/icons/$CHANGESTATEONE.png		$CHANGESTATEONE		playerctl --player "$PLAYERONE" play-pause
			IMG:$HOME/xmenu/icons/Next.png				Next				playerctl --player "$PLAYERONE" next
	$PLAYERTWO		
		$FORMATTWO	
			IMG:$HOME/xmenu/icons/Previous.png			Previous			playerctl --player "$PLAYETWO" previous
			IMG:$HOME/xmenu/icons/$CHANGESTATETWO.png		$CHANGESTATETWO		playerctl --player "$PLAYETWO" play-pause
			IMG:$HOME/xmenu/icons/Next.png				Next				playerctl --player "$PLAYETWO" next
	$PLAYERTHREE		
		$FORMATTHREE	
			IMG:$HOME/xmenu/icons/Previous.png			Previous			playerctl --player "$PLAYERTHREE" previous
			IMG:$HOME/xmenu/icons/$CHANGESTATETHREE.png	$CHANGESTATETHREE	playerctl --player "$PLAYERTHREE" play-pause
			IMG:$HOME/xmenu/icons/Next.png				Next				playerctl --player "$PLAYERTHREE" next
EOF
