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
proc valid_int_entry { pref vcur vback } {
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
