# -*- mode: python; coding: utf-8 -*-
# Gnome UI
# Copyright (C) 2008 David Härdeman <david@hardeman.nu>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

import sys, time, thread, threading
if __name__ == '__main__':
	from offlineimap import imapserver, repository, folder, mbnames, threadutil, version, syncmaster, accounts
	from offlineimap.threadutil import MultiLock
from offlineimap import version, threadutil
from UIBase import UIBase

# Temporarily make gtk emit exceptions insted of warnings
import warnings
warnings.filterwarnings('error', module='gtk')
try:
	import pygtk
	pygtk.require('2.0')
	import gobject
	import gtk
	import gnome
	if gtk.pygtk_version[0] != 2 or gtk.pygtk_version[1] < 10:
		raise Exception, 'Invalid pygtk version'
	gtk.gdk.threads_init()
	usable = True
except:
	usable = False
warnings.resetwarnings()

# FIXME: Custom icons
GNOME_UI_ICON = 'stock_mail-send-receive'
GNOME_UI_ACTIVE_ICONS = [ 'stock_mail-forward', 'stock_mail-reply' ]



class GnomeUIAboutDialog(gtk.AboutDialog):
	def __init__(self, ui):
		self.ui = ui
		self.closed = threading.Event()
		self.closed.set()
		gtk.AboutDialog.__init__(self)
		gtk.about_dialog_set_url_hook(lambda dialog,url: \
					      gnome.url_show(url))
		gtk.about_dialog_set_email_hook(lambda dialog,url: \
						gnome.url_show("mailto:" + url))

		self.connect('delete-event', self.delete_cb)
		self.connect('response', self.response_cb)
		self.set_name(version.productname)
		self.set_version(version.versionstr)
		self.set_copyright(version.copyright)
		self.set_license(version.license)
		self.set_wrap_license(True)
		self.set_website(version.homepage)
		self.set_website_label(version.homepage)
		author = "%s <%s>" % (version.author,
				      version.author_email)
		self.set_authors([author])
		self.set_logo_icon_name(GNOME_UI_ICON)
		self.set_program_name(version.productname)

	def response_cb(self, arg = None, argb = None):
		self.ui.debug("GnomeUIAboutDialog.response_cb()")
		self.close()
		return False

	def delete_cb(self, widget, event):
		self.ui.debug("GnomeUIAboutDialog.delete_cb()")
		# Prevents widget destruction
		return True

	def close(self):
		self.ui.debug("GnomeUIAboutDialog.close()")
		self.hide_all()
		self.closed.set()

	def open(self):
		self.ui.debug("GnomeUIAboutDialog.open()")
		self.show_all()
		self.closed.clear()


class GnomeUIStatusIcon(gtk.StatusIcon):
	STATE_ACTIVE = 1
	STATE_IDLE   = 2

	def __init__(self, ui):
		self.ui = ui
		self.closed = threading.Event()
		gtk.StatusIcon.__init__(self)
		self.connect('activate', self.ui.log.open)
		self.connect('popup-menu', self.popup_cb)
		self.animate_counter = 0
		self.state = None

		self.menu = gtk.Menu()

		self.mstart = gtk.ImageMenuItem("_Start Sync")
		img = gtk.image_new_from_stock(gtk.STOCK_EXECUTE, gtk.ICON_SIZE_MENU)
		self.mstart.set_image(img)
		self.mstart.connect('activate', self.ui.stopsleep_cb)
		self.mstart.set_sensitive(False)
		self.menu.append(self.mstart)

		mlog = gtk.ImageMenuItem("Show _Log")
		img = gtk.image_new_from_stock(gtk.STOCK_INFO, gtk.ICON_SIZE_MENU)
		mlog.set_image(img)
		mlog.connect('activate', self.ui.log.open)
		self.menu.append(mlog)

		sep = gtk.SeparatorMenuItem()
		self.menu.append(sep)

		mabout = gtk.ImageMenuItem(gtk.STOCK_ABOUT)
		mabout.connect('activate', self.ui.about.open)
		self.menu.append(mabout)

		mquit = gtk.ImageMenuItem(gtk.STOCK_QUIT)
		mquit.connect('activate', self.quit_cb)
		self.menu.append(mquit)

		self.set_state(GnomeUIStatusIcon.STATE_ACTIVE)
		self.set_visible(True)

	def set_title(self, title):
		self.ui.debug("GnomeUIStatusIcon.set_title(%s)" % title)
		self.set_tooltip(version.productname + ": " + title)

	def set_state(self, state):
		self.ui.debug("GnomeUIStatusIcon.set_state(%i)" % state)
		if self.state == state:
			return

		if state == GnomeUIStatusIcon.STATE_ACTIVE:
			gobject.timeout_add(500, self.animate_cb)
			self.set_title("active")
			self.mstart.set_sensitive(False)
			self.state = state
		elif state == GnomeUIStatusIcon.STATE_IDLE:
			self.set_from_icon_name(GNOME_UI_ICON)
			self.set_title("idle")
			self.mstart.set_sensitive(True)
			self.state = state

	def quit_cb(self, notused = None):
		self.ui.debug("GnomeUIStatusIcon.quit_cb()")
		self.close()
		self.ui.uiexit = True
		self.ui.quit_cb()

	def animate_cb(self):
		self.ui.debug("GnomeUIStatusIcon.animate_cb()")
		if self.state != GnomeUIStatusIcon.STATE_ACTIVE:
			return False

		icon = GNOME_UI_ACTIVE_ICONS[self.animate_counter % len(GNOME_UI_ACTIVE_ICONS)]
		self.set_from_icon_name(icon)
		self.animate_counter += 1
		return True

	def popup_cb(self, widget, button, time, data = None):
		self.ui.debug("GnomeUIStatusIcon.popup_cb()")
		if button != 3:
			return
		self.menu.show_all()
		self.menu.popup(None, None, None, 3, time)
		return False

	def close(self):
		self.ui.debug("GnomeUIStatusIcon.close()")
		self.set_visible(False)
		self.closed.set()

	def open(self):
		self.ui.debug("GnomeUIStatusIcon.open()")
		self.set_visible(True)
		self.closed.clear()



class GnomeUIPasswordDialog(gtk.Dialog):
	def __init__(self, ui):
		# DIALOG = +------+---------+
		#          | ICON | MESSAGE |
		#          |      |         |
		#          |      | INPUT   |
		#          +------+---------+
		#          |        BUTTONS |
		#          +----------------+
		self.ui = ui
		self.closed = threading.Event()
		self.closed.set()
		self.pw = None
		self.pwready = threading.Event()
		gtk.Dialog.__init__(self,
				    version.productname + \
				    " password prompt",
				    None,
				    gtk.DIALOG_NO_SEPARATOR,
				    # BUTTONS
				    (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
				     gtk.STOCK_OK, gtk.RESPONSE_OK))
		self.set_decorated(False)
		self.set_resizable(False)
		self.set_position(gtk.WIN_POS_CENTER_ALWAYS)
		self.set_border_width(16)
		self.connect('response', self.done_cb)

		# Split between ICON and MESSAGE + INPUT
		mainsplit = gtk.HBox(False, 8)
		self.vbox.pack_start(mainsplit, False, False, 0)

		# ICON
		icon = gtk.image_new_from_stock(gtk.STOCK_DIALOG_AUTHENTICATION,
						gtk.ICON_SIZE_DIALOG)
		valign = gtk.VBox(False, 0) # to top-align the icon
		valign.pack_start(icon, False, False, 0)
		mainsplit.pack_start(valign, False, False, 0)

		# Split between MESSAGE and INPUT
		split = gtk.VBox(False, 0)
		mainsplit.pack_start(split, True, True, 0)

		# MESSAGE
		self.msg = gtk.Label()
		lalign = gtk.HBox(False, 0) # to left-align the label
		lalign.pack_start(self.msg, False, False, 0)
		split.pack_start(lalign, False, False, 0)

		# INPUT
		input = gtk.HBox(False, 16)
		split.pack_start(input, True, True, 32)

		pwlabel = gtk.Label("Password:")
		input.pack_start(pwlabel, False, False, 0)

		self.pwentry = gtk.Entry()
		self.pwentry.set_visibility(False)
		self.pwentry.set_activates_default(True)
		input.pack_start(self.pwentry, True, True, 0)

	def done_cb(self, dialog, response):
		self.ui.debug("GnomeUIPasswordDialog.done_cb()")
		self.pw = None
		if response == gtk.RESPONSE_OK:
			self.pw = self.pwentry.get_text()
		self.pwready.set()
		self.close()
		return False

	def close(self):
		self.ui.debug("GnomeUIPasswordDialog.close()")
		self.hide_all()
		self.closed.set()

	def open(self, markup = ''):
		self.ui.debug("GnomeUIPasswordDialog.open()")
		self.pwentry.set_text('')
		self.pwready.clear()
		self.msg.set_markup(markup)
		self.set_default_response(gtk.RESPONSE_OK)
		self.show_all()
		self.closed.clear()


class GnomeUILogWindow(gtk.Dialog):
	def __init__(self, ui):
		self.ui = ui
		self.closed = threading.Event()
		self.closed.set()
		self.max_lines = 64 * 1024
		self.autoscroll = True
		gtk.Dialog.__init__(self,
				    version.productname + " log",
				    None,
				    0,
				    #gtk.DIALOG_NO_SEPARATOR,
				    (gtk.STOCK_CLEAR, gtk.RESPONSE_CANCEL,
				     gtk.STOCK_OK, gtk.RESPONSE_OK))
		self.set_default_size(640, 480)
		self.connect('response', self.response_cb)
		self.connect('delete-event',self.close_on_delete_cb)

		box = gtk.VBox(False, 8)
		self.vbox.pack_start(box, True, True, 0)

		title = gtk.HBox(False, 8)
		img = gtk.image_new_from_icon_name(GNOME_UI_ICON,
						   gtk.ICON_SIZE_DIALOG)
		label = gtk.Label()
		label.set_markup("<big><b>" + \
				 version.productname + " Log" + \
				 "</b></big>\n" + \
				 "Recent log messages:")
		title.pack_start(img, False, False, 0)
		title.pack_start(label, False, False, 0)
		box.pack_start(title, False, False, 8)

		self.sw = gtk.ScrolledWindow()
		self.sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
		self.tv = gtk.TextView()
		self.tv.set_wrap_mode(gtk.WRAP_WORD)
		self.tv.set_pixels_below_lines(6)
		self.tv.set_pixels_inside_wrap(0)
		self.tv.set_editable(False)
		self.tv.set_cursor_visible(False)
		self.sw.add(self.tv)
		self.tb = self.tv.get_buffer()
		self.tb.connect('insert-text', self.insert_cb)
		box.pack_start(self.sw, True, True, 8)

		abox = gtk.HBox(False, 8)
		box.pack_start(abox, False, False, 16)

		self.entry_autoscroll = gtk.CheckButton("_Autoscroll")
		self.entry_autoscroll.set_active(self.autoscroll)
		self.entry_autoscroll.connect('toggled', self.toggle_scroll_cb)
		abox.pack_start(self.entry_autoscroll, True, True, 0)

		label = gtk.Label("Max Lines:")
		abox.pack_start(label, False, False, 0)

		adj = gtk.Adjustment(self.max_lines, 1, 65536, 1, 1024, 1024)
		self.entry_lines = gtk.SpinButton(adj, 1.0, 0)
		self.entry_lines.set_numeric(True)
		self.entry_lines.set_wrap(False)
		self.entry_lines.set_snap_to_ticks(True)
		self.entry_lines.connect('value-changed', self.line_change_cb)
		abox.pack_start(self.entry_lines, False, False, 0)

	def scroll(self):
		self.ui.debug("GnomeUILogWindow.scroll()")
		if self.autoscroll:
			end = self.tb.get_end_iter()
			self.tv.scroll_to_iter(end, 0.0, True, 1.0, 1.0)

	def delete_lines(self):
		self.ui.debug("GnomeUILogWindow.delete_lines()")
		lines = self.tb.get_line_count()
		diff = lines - self.max_lines - 1
		if diff > 0:
			start = self.tb.get_start_iter()
			end = self.tb.get_iter_at_line(diff)
			self.tb.delete(start, end)

	def line_change_cb(self, spinbutton):
		self.ui.debug("GnomeUILogWindow.line_change_cb()")
		self.max_lines = self.entry_lines.get_value_as_int()
		self.delete_lines()

	def toggle_scroll_cb(self, togglebutton):
		self.ui.debug("GnomeUILogWindow.toggle_scroll_cb()")
		self.autoscroll = togglebutton.get_active()
		self.scroll()

	def insert_cb(self, textbuffer, iter, text, length):
		self.ui.debug("GnomeUILogWindow.insert_cb()")
		self.scroll()

	def add_msg(self, msg):
		self.ui.debug("GnomeUILogWindow.add_msg()")
		gtk.gdk.threads_enter()
		end = self.tb.get_end_iter()
		self.tb.insert(end, msg.strip() + "\n")
		self.delete_lines()
		# the insert-text signal will take care of scrolling...
		gtk.gdk.threads_leave()
		return False

	def close_on_delete_cb(self, widget, event):
		# response_cb will handle everything
		return True

	def response_cb(self, widget, response):
		self.ui.debug("GnomeUILogWindow.response_cb()")
		if response == gtk.RESPONSE_CANCEL:
			self.tb.set_text('')
			return False
		else:
			self.close()
			# Prevent widget destruction
			return True

	def close(self):
		self.ui.debug("GnomeUILogWindow.close()")
		self.hide_all()
		self.closed.set()

	def open(self, notused = None):
		self.ui.debug("GnomeUILogWindow.open()")
		self.show_all()
		self.closed.clear()


class GnomeUIWarningWindow(gtk.Dialog):
	def __init__(self, ui):
		self.ui = ui
		self.closed = threading.Event()
		self.closed.set()
		gtk.Dialog.__init__(self,
				    version.productname + " WARNING",
				    None,
				    0,
				    #gtk.DIALOG_NO_SEPARATOR,
				    (gtk.STOCK_OK, gtk.RESPONSE_OK))
		self.set_default_size(640, 480)
		self.connect('response', self.response_cb)

		box = gtk.VBox(False, 8)
		self.vbox.pack_start(box, True, True, 0)

		title = gtk.HBox(False, 8)
		img = gtk.image_new_from_stock(gtk.STOCK_DIALOG_WARNING,
					       gtk.ICON_SIZE_DIALOG)
		label = gtk.Label()
		label.set_markup("<big><b>" + \
				 version.productname + " Warning" + \
				 "</b></big>\n" + \
				 "The following errors have occurred:")
		title.pack_start(img, False, False, 0)
		title.pack_start(label, False, False, 0)
		box.pack_start(title, False, False, 8)

		self.sw = gtk.ScrolledWindow()
		self.sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
		self.tv = gtk.TextView()
		self.tv.set_wrap_mode(gtk.WRAP_WORD)
		self.tv.set_pixels_below_lines(6)
		self.tv.set_pixels_inside_wrap(0)
		self.tv.set_editable(False)
		self.tv.set_cursor_visible(False)
		self.sw.add(self.tv)
		self.tb = self.tv.get_buffer()
		self.tb.connect('insert-text', self.insert_cb)
		box.pack_start(self.sw, True, True, 8)

	def insert_cb(self, textbuffer, iter, text, length):
		self.ui.debug("GnomeUIWarningWindow.insert_cb()")
		end = self.tb.get_end_iter()
		self.tv.scroll_to_iter(end, 0.0, True, 1.0, 1.0)

	def add_msg(self, msg):
		self.ui.debug("GnomeUIWarningWindow.add_msg()")
		end = self.tb.get_end_iter()
		self.tb.insert(end, msg.strip() + "\n")
		self.open()
		return False

	def response_cb(self, widget = None, event = None):
		self.ui.debug("GnomeUIWarningWindow.response_cb()")
		self.close()
		# Prevent widget destruction
		return True

	def close(self):
		self.ui.debug("GnomeUIWarningWindow.close()")
		self.hide_all()
		self.tb.set_text('')
		self.closed.set()

	def open(self):
		self.ui.debug("GnomeUILogWindow.open()")
		self.show_all()
		self.closed.clear()


class GnomeUIThread:
	def __init__(self):
		# General
		self.dbg       = False
		self.lock      = threading.Lock()
		self.thread    = None
		self.uiexit    = False # Was an exit action initiated by the UI?
		self.stopsleep = threading.Event()
		self.exitsync  = threading.Event()
		gtk.window_set_default_icon_name(GNOME_UI_ICON)

		# Widgets
		self.about     = GnomeUIAboutDialog(self)
		self.log       = GnomeUILogWindow(self)
		self.warn      = GnomeUIWarningWindow(self)
		self.pwdialog  = GnomeUIPasswordDialog(self)
		self.icon      = GnomeUIStatusIcon(self)

	def start(self):
		self.debug("GnomeUIThread.start()")
		self.lock.acquire()
		if self.thread is None:
			self.exitsync.clear()
			self.thread = threadutil.ExitNotifyThread(target = self.uiloop,
								  name = "GnomeUIThread")
			self.thread.setDaemon(True)
			self.thread.start()
		self.lock.release()

	def stop(self):
		self.debug("GnomeUIThread.stop()")
		self.lock.acquire()
		if self.thread is not None:
			gobject.idle_add(self.quit_cb, None)
			# self.thread.join won't work for some reason...
			self.exitsync.wait()
			self.thread = None
		self.lock.release()

	def getpass(self, accountname, config, errmsg = None):
		self.debug("GnomeUIThread.getpass()")
		pw = None
		self.lock.acquire()
		if self.thread is not None:
			gobject.idle_add(self.pw_cb, accountname, config, errmsg)
			self.pwdialog.pwready.wait()
			self.pwdialog.pwready.clear()
			pw = self.pwdialog.pw
			self.pwdialog.pw = None
		self.lock.release()
		return pw

	def add_msg(self, msg):
		self.debug("GnomeUIThread.add_msg(%s)" % msg)
		self.lock.acquire()
		if self.thread is not None:
			gobject.idle_add(self.log.add_msg, msg)
		self.lock.release()

	def add_warning(self, msg):
		self.debug("GnomeUIThread.add_warning(%s)" % msg)
		self.add_msg('WARNING: ' + msg)
		self.lock.acquire()
		if self.thread is not None:
			gobject.idle_add(self.warn.add_msg, msg)
		self.lock.release()

	# Internal methods follow
	def enable_debug(self, enable = True):
		self.dbg = enable

	def debug(self, msg):
		if self.dbg:
			sys.stderr.write("GnomeUI-DEBUG: " + msg + "\n")

	def stopsleep_cb(self, data = None):
		self.debug("GnomeUIThread.stopsleep_cb()")
		self.stopsleep.set()
		return False

	basemarkup = "<big><b>Enter %s password</b></big>\n\n" + \
		"A password is needed to access account \"%s\"."

	basemarkuperr = basemarkup + "\n\n" + \
		"<span foreground=\"red\">%s</span>"

	def pw_cb(self, accountname, config = None, errmsg = None):
		self.debug("GnomeUIThread.pw_cb()")

		if errmsg is None:
			markup = self.basemarkup % \
				(version.productname,
				 accountname)
		else:
			markup = self.basemarkuperr % \
				(version.productname,
				 accountname,
				 errmsg)

		self.pw = self.pwdialog.open(markup)
		return False

	def quit_cb(self, data = None):
		self.debug("GnomeUIThread.quit_cb()")
		self.about.close()
		self.log.close()
		self.warn.close()
		self.pwdialog.close()
		self.icon.close()
		gtk.main_quit()
		return False

	def uiloop(self):
		self.debug("GnomeUIThread.uiloop() - begin")
		gtk.main()
		self.debug("GnomeUIThread.uiloop() - end (%s)" % repr(self.uiexit))
		if self.uiexit:
			thread.interrupt_main()
		self.exitsync.set()
		#thread.exit()


class GnomeUI(UIBase):
	def __init__(self, config, verbose = 0):
		UIBase.__init__(self, config, verbose)

	def isusable(self):
		return usable

	def init_banner(self):
		self.ui = GnomeUIThread()
		self.ui.start()
		UIBase.init_banner(self)

	def getpass(self, accountname, config, errmsg = None):
		return self.ui.getpass(accountname, config, errmsg)

	def _display(self, msg):
		self.ui.add_msg(msg)

	def warn(self, msg, minor = 0):
		if minor:
			self.ui.add_msg('warning: ' + str(msg))
		else:
			self.ui.add_warning(str(msg))

	def sleep(self, secs, siglistener):
		self.ui.debug("GnomeUI.sleep(%i)" % secs)
		msg = "Sleeping for %d minutes" % (secs / 60)
		if secs % 60 > 0:
			msg += " and %02d seconds" % (secs % 60)
		self.ui.add_msg(msg)
		self.ui.icon.set_state(GnomeUIStatusIcon.STATE_IDLE)
		self.ui.stopsleep.clear()
		UIBase.sleep(self, secs, siglistener)

	def sleeping(self, secs, remainingsecs):
		# Return values
		# 0 if timeout expired
		# 1 if there is a request to cancel the timer
		# 2 if there is a request to abort the program
		self.ui.icon.set_title("sleeping for %dm%02ds"  % \
				       (remainingsecs / 60, remainingsecs % 60))
		if secs > 0:
			self.ui.stopsleep.wait(secs)
			if self.ui.stopsleep.isSet():
				return 1
		else:
			self.ui.icon.set_state(GnomeUIStatusIcon.STATE_ACTIVE)
		return 0

	def terminate(self, exitstatus = 0, errortitle = None, errormsg = None):
		self.ui.debug("Terminating")
		if not self.ui.uiexit and errormsg is not None:
			if errortitle is not None:
				self.ui.add_warning('ERROR: %s\n\n%s\n'%(errortitle, errormsg))
			else:
				self.ui.add_warning('%s\n' % errormsg)

		# Give the user a chance to read any warnings
		self.ui.warn.closed.wait()
		if not self.ui.uiexit:
			self.ui.stop()
		UIBase.terminate(self, exitstatus, errortitle, errormsg)

	def locked(self):
		self.ui.warn("Another OfflineIMAP is running with the same metadatadir; exiting.", 1)

if __name__ == '__main__':
	import time
	from offlineimap.accounts import SigListener
	print "MAIN: Begin tests"

	siglistener = SigListener()

	x = GnomeUI(None)
	x.init_banner()
	x.ui.enable_debug(True)

	warnings = 5
	messages = 5

	x.warn("Printing %i Warnings" % warnings)
	for i in range(warnings):
		x.warn("Warning " + str(i + 1))
		time.sleep(1)
	
	x.warn("Sleeping 999 secs")
	x.warn("Select \"Start Sync\" from status icon context menu to continue")
	x.sleep(999, siglistener)

	x.warn("Showing log window")
	x.ui.log.open()

	x.warn("Printing %i Log Messages" % messages)
	for i in range(messages):
		x.ui.add_msg("Line " + str(i + 1) + " of log text")
		time.sleep(1)
	x.warn("Sleeping 10 secs")
	x.sleep(10, siglistener)

	x.warn("Showing about - close to continue")
	x.ui.about.open()
	x.ui.about.closed.wait()

	x.warn("Getting a password")
	password = x.ui.getpass("TEST", None, None)
	if password is not None:
		x.warn("Password was " + password)
	else:
		x.warn("No password given")

	x.warn("Sleeping 3 secs")
	x.sleep(3, siglistener)
	x.warn("Trying a password again with error message")
	password = x.ui.getpass("Test", None, "Error message")
	if password is not None:
		x.warn("Password was " + password)
	else:
		x.warn("No password given")

	x.warn("Terminating, close warning window to exit")
	x.terminate()

