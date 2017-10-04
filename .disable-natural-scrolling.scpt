    tell application "System Preferences"
	activate
	set current pane to pane "com.apple.preference.trackpad"
    end tell

    tell application "System Events"
	    tell process "System Preferences"
		    click radio button "Scroll & Zoom" of tab group 1 of window "Trackpad"
		
		    set theCheckbox to checkbox 1 of tab group 1 of window "Trackpad"
		
    		tell theCheckbox
	    		set checkBoxStatus to value of theCheckbox as boolean
		    	if checkBoxStatus is false then click theCheckbox
		    	# Set to "if checkBoxStatus is true" to disable
    		end tell
		
	    end tell
    end tell

    tell application "System Preferences"
	    quit
    end tell
