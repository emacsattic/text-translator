#
#	$Id: Makefile,v 1.11 2007/12/02 20:49:37 s122139 Exp $
#

SHELL			=	/bin/sh
MKDIR			=	mkdir -p
INSTALL			=	/usr/bin/install -c
EMACS			=	emacs

PACKAGE			=	text-translator

prefix			=	/usr/local
LISPDIR			=	$(prefix)/share/emacs/site-lisp
INSTALLDIR		=	$(LISPDIR)/$(PACKAGE)

EL			=	text-translator.el         \
				text-translator-load.el    \
				text-translator-popup.el   \
				text-translator-pos-tip.el \
				text-translator-sites.el   \
				text-translator-vars.el    \
				text-translator-window.el

EL_NOT_COMPILE	=	text-translator-load.el
ELC				=	$(EL:.el=.elc)

DISTDIR			=	$(PACKAGE)-$(VERSION)
TARBALL			=	$(PACKAGE)-$(VERSION).tar.gz
FILES			=	Makefile README.en README.ja $(EL) $(EL_NOT_COMPILE)

BATCH_FLAGS		=	-batch -q -no-site-file


all : compile-el

compile-el : $(ELC)

install : compile-el
	@if [ ! -d $(INSTALLDIR) ]; then \
		$(MKDIR) $(INSTALLDIR); \
	fi
	$(INSTALL) -m 644 $(EL) $(ELC) $(EL_NOT_COMPILE) $(INSTALLDIR)

clean :
	-$(RM) $(ELC) *.patch.gz $(PACKAGE)-*.tar.bz2

tarball: Makefile
	$(MAKE) VERSION=`$(EMACS) $(BATCH_FLAGS) \
		-l text-translator-vars.el \
		-eval '(princ text-translator-version)' 2> /dev/null` dist

dist : $(FILES)
	@$(MKDIR) $(DISTDIR)
	-@cp -a $(FILES) $(DISTDIR)
	tar -zcf $(TARBALL) $(DISTDIR)
	-@$(RM) -r $(DISTDIR)

patch :
	@for file in $(FILES) ; \
	do \
		patch=$$file.patch ; \
		cvs diff $$file > $$patch ; \
		if [ -s $$patch ] ; then \
			gzip -f $$patch ; \
		else \
			$(RM) $$patch ; \
		fi \
	done

%.elc: %.el
	$(EMACS) $(BATCH_FLAGS) \
		-eval '(setq load-path (cons "." load-path))' -f batch-byte-compile $<
