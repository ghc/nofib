
include $(TOP)/nofib/mk/site.mk

#-----------------------------------------------------------------------------
# General utilities

SHELL	= /bin/sh
RM	= rm -f
TIME	= time
STRIP 	= strip
SIZE	= size

#-----------------------------------------------------------------------------
# Haskell utilities

ifdef UseInstalledUtils
  RUNSTDTEST = runstdtest
else
  RUNSTDTEST = $(TOP)/glafp-utils/runstdtest/runstdtest
endif

#-----------------------------------------------------------------------------
# The 'all' target prints out the corrent configuration and builds runtests.

all ::
	@echo HC = $(HC)
	@echo HCFLAGS = $(HCFLAGS)
	@echo RUNTESTFLAGS = $(RUNTESTFLAGS)

print_% ::
	@echo HCFLAGS_$* = $(HCFLAGS_$*)

all :: $(foreach way,$(WAYS),print_$(way)) runtests

#-----------------------------------------------------------------------------
# Subdirs stuff.

ifdef SUBDIRS
  clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) clean; \
	done
  veryclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) veryclean; \
	done
  runtests::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ; do \
	  $(MAKE) -C $$i $(MFLAGS) runtests; \
	done
endif

#-----------------------------------------------------------------------------
# Cleaning things.

clean ::
	$(RM) *.CKP *.ln *.BAK *.bak *.o core a.out ,* *.a .emacs_* *.hi
	$(RM) tags TAGS *.ind *.ilg *.idx *.idx-prev *.aux *.aux-prev *.dvi
	$(RM) *.log *.toc *.lot *.lof *.blg *.info *.itxi *.itex *.cb errs

veryclean ::
	$(RM) .??*~ *~ *.orig *.rej

#-----------------------------------------------------------------------------
# Nofib program targets.

ifdef PROG

ifndef SRCS
  SRCS = Main.hs
endif

OBJS = $(patsubst %.lhs, %.o, $(patsubst %.hs, %.o, $(SRCS)))

define COMPILE
	@echo === compiling $(PROG)/$@ $(EXTRA_HCFLAGS) ===
	@$(TIME) $(HC) $(HCFLAGS) $(EXTRA_HCFLAGS)  -o $@ -c $< \
	  $(HCFLAGS_$(patsubst .%,%,$(suffix $(basename $@))))
	@echo === size of $(PROG)/$@ ===
	@$(SIZE) $@
endef

%.normal.o %.mc.o %.mr.o %.mt.o %.mp.o %.mg.o %.2s.o %.1s.o %.du.o \
%.a.o %.b.o %.c.o %.d.o %.e.o %.f.o %.g.o %.h.o %.i.o %.j.o %.k.o \
%.l.o %.m.o %.n.o %.o.o %.p.o %.A.o %.B.o : %.hs
	$(COMPILE)

%.normal.o %.mc.o %.mr.o %.mt.o %.mp.o %.mg.o %.2s.o %.1s.o %.du.o \
%.a.o %.b.o %.c.o %.d.o %.e.o %.f.o %.g.o %.h.o %.i.o %.j.o %.k.o \
%.l.o %.m.o %.n.o %.o.o %.p.o %.A.o %.B.o : %.lhs
	$(COMPILE)

$(PROG)_% : $(OBJS:.o=.%.o)
	@echo === linking $@ $(EXTRA_HCFLAGS) ===
	@$(TIME) $(HC) $(HCFLAGS) $(EXTRA_HCFLAGS) -o $@ $^ $(LIBS)
	@$(STRIP) $@
	@echo === size of $@ ===
	@$(SIZE) $@

runtest_% : $(PROG)_%
	@echo === running $< $(EXTRA_RUNTESTFLAGS) ===
	@$(TIME) $(RUNSTDTEST) ./$< \
	  $(addprefix -i ,$(wildcard $(PROG).stdin)) \
	  $(addprefix -o1 ,$(wildcard $(PROG).stdout)) \
	  $(addprefix -o2 ,$(wildcard $(PROG).stderr)) \
	  $(RUNTESTFLAGS) $(EXTRA_RUNTESTFLAGS)

runtests :: $(foreach way,$(WAYS),$(PROG)_$(way) runtest_$(way))

clean ::
	$(RM) $(foreach way,$(WAYS),$(PROG)_$(way))

endif
