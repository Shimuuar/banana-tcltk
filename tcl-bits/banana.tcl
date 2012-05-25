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
    proc callback { pref n } {
	puts [ concat $pref [ $n index current ] ]
    }
    bind $n <<NotebookTabChanged>> [list callback $pref $n ]
}
