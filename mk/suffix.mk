#################################################################################
#
#			    nofib/mk/suffix.mk
#
#		Extra suffix rules for nofib project
#
#################################################################################

# We override the compile suffix rules so that
# we can gather time and size information

define COMPILE
	@echo ==nofib==  $(NOFIB_PROG): time to compile $@ follows...
	@echo $(HC) $(HC_OPTS) -c $< -o $@
	@time $(HC) $(HC_OPTS) -c $< -o $@
	@if (test -f $@); then \
		echo ==nofib== $(NOFIB_PROG): size of $@ follows... ; \
		$(SIZE) $@ ; \
	fi;
endef

%.$(way_)o : %.hs
	$(COMPILE)
			 
%.$(way_)o : %.lhs
	$(COMPILE)
			 

