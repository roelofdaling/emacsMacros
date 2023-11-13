# ssw-psflow-emacs-log-navigation

<ul>
<li>This file contains a number of emacs-lisp functions to facilitate navigating through a large PSFLOW log file. It is in particular intended for
log-files obtained with the Coflow <tt>Debug</tt>-logging level</li>
<li>To activate this functionality use the emacs 'load-file' command to load this file. It is most convenient to do this in
you init.el file. For example, by adding the following two lines:
<tt>
(add-to-list 'load-path "&ltpathToYourClone&gt")
(load "psf-log-navigation.el")  
</tt>
</li>  
</ul>
When <tt>psf-log-navigation.el</tt> has been loaded the functions to navigate can be called either via the PSFLOW-log menu 
or via the emacs mini-buffer. The menu-items are enabled/disabled according to the content of the current buffer: For example
if no IPF log messages are found the IPF-related meny is disabled. 

All commands start with <tt>psf</tt>. So to run via the mini-buffer start with <Esc-x> and enter "psf"  and hit tab for auto-completion.

Well-names, solution-spaces and reservoir-names arguments, which are input to some of the functions, can also be entered using auto-completion.  

Function types:
<dl>
<dt>
  <b>Log-regions</b>
  <dd>Use the emacs org-mode functionality to annotate regions of the log-file such that they can collapsed and opened using the <TAB> key</dd>
</dt>
<dt>
  <b>UTH-navigation</b>
  <dd> Functions related to finding well IPR-data and well targets</dd>
  <dd> Functions to open IMEX .dat, .out and .obdbg files </dd>
</dt>
<dt>
  <dd> Functions to navigate between IPF timesteps and solution spaces.</dd>
</dt>    
</dl>
