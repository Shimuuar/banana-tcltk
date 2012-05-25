# Collection of helper functions
#
# Note that automatically generated variable names always have form
#   var_[number]
# Such variable names must not appear here



# Validate entry for interger entry
#
#   pref   - unique command prefix
#   v_cur  - current variable
#   v_back - backup variable
proc entry_validate_int { pref vcur vback } {
    upvar $vcur  cur
    upvar $vback back
    # no need to do anything if entry didn't change
    if { $cur != $back } {
	if [ string is integer $cur ] {
	    # OK send new event
	    set back $cur
	    puts [ concat $pref $cur ]
	} else {
	    # Restore
	    set cur $back
	}
    }
}


# Generate event whenever tab in notebook widget is changed
#
#   pref - event prefix
#   n    - notebook widget
proc notebook_event_tab { pref n } {
    proc notebook_event_tab_callback { pref n } {
	puts [ concat $pref [ $n index current ] ]
    }
    bind $n <<NotebookTabChanged>> [list notebook_event_tab_callback $pref $n ]
}


proc checkbutton_event_toggle { pref n st } {
    # Callback
    proc checkbutton_event_toggle_callback { pref n } {
	puts [ concat $pref [ $n instate selected ] ]
    }
    $n configure -command [list checkbutton_event_toggle_callback $pref $n]
    # Set initial state and ping back
    if $st {
	$n state selected
    }
    puts [ concat $pref [ $n instate selected ] ]
}