.PHONY: all, clean

# SECONDARY designates all implicit intermediate files as secondary targets. This means
# they won't be deleted when make runs. This can be kind of annoying if the program 
# generating the intermediate files errors out, but it's interesting to have these files
# around.
.SECONDARY:

SOURCES = $(wildcard *.w)
PROGRAMS = $(SOURCES:%.w=%)

all: $(PROGRAMS)

clean:
	rm -f *.a *.o *.ll *.bc *.s *.w32 *.wo *.wl *.wp $(PROGRAMS)

$(foreach program,$(PROGRAMS),\
  $(eval $(program):$(program).o))

$(PROGRAMS):
	clang++ $(LDFLAGS) $^ -o $@

include common.mk
include 64.mk
