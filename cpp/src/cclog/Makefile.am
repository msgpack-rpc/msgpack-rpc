
lib_LTLIBRARIES = libcclog.la

libcclog_la_includedir = $(includedir)/cclog

libcclog_la_SOURCES = \
		cclog.cc \
		cclog_null.cc \
		cclog_ostream.cc \
		cclog_syslog.cc \
		cclog_tty.cc

libcclog_la_include_HEADERS = \
		cclog.h \
		cclog_null.h \
		cclog_ostream.h \
		cclog_syslog.h \
		cclog_tty.h

PREP_SOURCE = \
		cclog.hmpl

PREP_TARGET = $(PREP_SOURCE:mpl=)
EXTRA_DIST = $(PREP_SOURCE)

%.h: %.hmpl
	$(MPLEX) $< -o $@

%.cc: %.ccmpl
	$(MPLEX) $< -o $@

prep: $(PREP_TARGET)

prepc:
	rm -f $(PREP_TARGET)

