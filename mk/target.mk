#################################################################################
#
#			target.mk
#
#		nofib standard target rules
#
#################################################################################


# Only do this in leaf directories (important, this)

nofib-dist-pre::
	-rm -rf $(SRC_DIST_DIR)
	-rm -f $(SRC_DIST_NAME).tar.gz
	(cd $(FPTOOLS_TOP_ABS)/nofib; find $(SRC_DIST_DIRS) -type d \( -name CVS -prune -o -name SRC -prune -o -name tests -prune -o -exec $(MKDIRHIER) $(SRC_DIST_DIR)/{} \; \) ; )
	(cd $(FPTOOLS_TOP_ABS)/nofib; find $(SRC_DIST_DIRS) -name CVS -prune -o -name SRC -prune -o -name tests -prune -o -name "*~" -prune -o -name ".cvsignore" -prune -o -type l -exec $(LN_S) $(FPTOOLS_TOP_ABS)/nofib/{} $(SRC_DIST_DIR)/{} \; )

ifeq "$(SUBDIRS)" ""
all ::
	@echo HC = $(HC)
	@echo HC_OPTS = $(HC_OPTS)
	@echo RUNTEST_OPTS = $(RUNTEST_OPTS)


all :: runtests
endif

# Bogosity needed here to cope with .exe suffix for strip & size files.
# (shouldn't have to be our problem.)
ifneq "$(HC_FAIL)" "YES"
$(NOFIB_PROG) : $(OBJS)
	@echo ==nofib== $(NOFIB_PROG): time to link $(NOFIB_PROG) follows...
	@$(TIME) $(HC) $(HC_OPTS) -o $@ $^ $(LIBS)
endif

ifneq "$(NOFIB_PROG)" ""
size :: $(NOFIB_PROG)
	@$(STRIP) $(NOFIB_PROG)$(exeext)
	@echo ==nofib== $(NOFIB_PROG): size of $(NOFIB_PROG) follows...
	@$(SIZE) $(NOFIB_PROG)$(exeext)

runtests :: $(NOFIB_PROG) size
	@echo ==nofib== $<: time to run $< follows...
	@$(TIME) $(RUNTEST) ./$< \
	  $(addprefix -i ,$(wildcard $(subst $(_way),,$(NOFIB_PROG)).stdin)) \
	  $(addprefix -o1 ,$(wildcard $(subst $(_way),,$(NOFIB_PROG)).stdout*)) \
	  $(addprefix -o2 ,$(wildcard $(subst $(_way),,$(NOFIB_PROG)).stderr*)) \
	  $(RUNTEST_OPTS)
else
size ::
	@:
runtests ::
	@:
endif

# Include standard boilerplate
# We do this at the end for cosmetic reasons: it means that the "normal-way"
# runtests will precede the "other-way" recursive invocations of make

include $(FPTOOLS_TOP)/mk/target.mk
