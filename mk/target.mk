#################################################################################
#
#			target.mk
#
#		nofib standard target rules
#
#################################################################################


# Only do this in leaf directories (important, this)
ifeq "$(SUBDIRS)" ""
all ::
	@echo HC = $(HC)
	@echo HC_OPTS = $(HC_OPTS)
	@echo RUNTEST_OPTS = $(RUNTEST_OPTS)


all :: runtests
endif

$(NOFIB_PROG) : $(OBJS)
	@echo ==nofib== $(NOFIB_PROG): time to link $(NOFIB_PROG) follows...
	@$(TIME) $(HC) $(HC_OPTS) -o $@ $^ $(LIBS)
	@if (test -f $@ ); then \
		$(STRIP) $@; \
		echo ==nofib== $(NOFIB_PROG): size of $(NOFIB_PROG) follows...; \
		$(SIZE) $@; \
	fi;

ifneq "$(NOFIB_PROG)" ""
runtests :: $(NOFIB_PROG)
	@echo ==nofib== $<: time to run $< follows...
	@$(TIME) $(RUNTEST) ./$< \
	  $(addprefix -i ,$(wildcard $(NOFIB_PROG).stdin)) \
	  $(addprefix -o1 ,$(wildcard $(NOFIB_PROG).stdout)) \
	  $(addprefix -o2 ,$(wildcard $(NOFIB_PROG).stderr)) \
	  $(RUNTEST_OPTS)
else
runtests ::
	@:
endif

# Include standard boilerplate
# We do this at the end for cosmetic reasons: it means that the "normal-way"
# runtests will precede the "other-way" recursive invocations of make

include $(FPTOOLS_TOP)/mk/target.mk
