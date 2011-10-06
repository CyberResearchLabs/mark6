#
# $Id: Makefile.am 1449 2011-09-26 14:36:03Z gbc $
#

SUBDIRS = . test

bin_PROGRAMS   = bmr_buffer bmr_record bmr_replay bmr_time

noinst_HEADERS = bmr_common.h bmr_files.h bmr_util.h bmr_server.h \
		 bmr_scan.h \
		 buffer_server.h record_server.h replay_server.h \
		 grab_task.h push_task.h \
		 save_task.h load_task.h \
		 vdif_epochs.h anal_util.h

bmr_time_SOURCES   = bmr_time_main.c bmr_time.c

bmr_buffer_SOURCES = buffer_main.c buffer_server.c \
		     buffer_parser.c buffer_info.c \
		     bmr_cache.c bmr_util.c bmr_time.c bmr_scan.c \
		     bmr_parser.c bmr_server.c bmr_listener.c \
		     grab_task.c grab_parser.c grab_info.c \
		     push_task.c push_parser.c push_info.c
bmr_buffer_CPPFLAGS = -DBMR_SERVER=1

bmr_record_SOURCES = record_main.c record_server.c \
		     record_parser.c record_info.c \
		     bmr_cache.c bmr_util.c bmr_time.c bmr_scan.c \
		     bmr_parser.c bmr_server.c bmr_listener.c \
		     grab_task.c grab_parser.c grab_info.c \
		     save_task.c save_parser.c save_info.c \
		     anal_util.c anal_m5be.c
bmr_record_CPPFLAGS = -DBMR_SERVER=2

bmr_replay_SOURCES = replay_main.c replay_server.c \
		     replay_parser.c replay_info.c \
		     bmr_cache.c bmr_util.c bmr_time.c bmr_scan.c \
		     bmr_parser.c bmr_server.c bmr_listener.c \
		     load_task.c load_parser.c load_info.c \
		     push_task.c push_parser.c push_info.c
bmr_replay_CPPFLAGS = -DBMR_SERVER=3

CLEANFILES = *_scan*

#
# eof
#
