# Collection of helper functions
#
# Note that automatically generated variable names always have form
#   var_[number]
# Such variable names must not appear here


# Make sane defaults for the file dialog. By default it shows all
# gazillion of hidden files which is pure insanity from usability
# standpoint.
#
# load the dialog once, otherwise setting the vars will not work
catch {tk_getOpenFile -with-invalid-argument}
# change the environment variables
namespace eval ::tk::dialog::file {
    variable showHiddenBtn 1
    variable showHiddenVar 0
}


# ================================================================
# Widgets
# ================================================================

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


# Generate event when checkbutton is toggled
#
#   pref - event prefix
#   n    - checkbox name
#   st   - initial state
proc checkbutton_event_toggle { pref n st } {
    # Callback
    proc checkbutton_event_toggle_callback { pref n } {
	puts [ concat $pref [ $n instate selected ] ]
    }
    # Set initial state
    if $st {
	$n state selected
    }
    $n configure -command [list checkbutton_event_toggle_callback $pref $n]
}



# ================================================================
# Dialogs
# ================================================================

proc dialog_file_worker { pref fun } {
    set x [ $fun ]
    if { $x != "" } {
	puts [concat $pref $x]
    }
}

# Dialog for open files
#
#  pref - event prefix
proc dialog_open_file {pref} {
    dialog_file_worker $pref tk_getOpenFile
}

# Dialog for open files
#
#  pref - event prefix
proc dialog_save_file {pref} {
    dialog_file_worker $pref tk_getSaveFile
}
