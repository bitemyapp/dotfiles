# where the w3 lisp files should go
prefix  = gnu_root
datadir = $(prefix):[lib]
confdir = $(prefix):[lib.emacs.w3]

ECHO       = write sys$output
RM         = delete
MKDIR      = create/dir

############## no user servicable parts beyond this point ###################
TARGETS = default.css

all: $(TARGETS)

install:
	if f$parse("$(confir)") .eqs. "" then $(MKDIR) $(confdir)
	copy/log *.info* $(confdir)
	- purge/log $(confdir)

distclean: clean
	$(RM) Makefile

clean:
	$(RM) *.dvi *.info* *.html

