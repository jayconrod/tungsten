SOURCES = $(wildcard *.w)
PROGRAMS = $(SOURCES:%.w=%)

LLVM_RUNTIME_SOURCE = ../llvm/runtime/runtime.ll

.PHONY: all, clean

.SECONDARY:

all: $(PROGRAMS)

clean:
	rm -f *.ll *.bc *.s *.wo *.wl *.wp $(PROGRAMS)

$(foreach program,$(PROGRAMS),\
    $(eval $(program):$(program).s))
$(PROGRAMS): runtime.s
	gcc $^ -o $@

runtime.ll: $(LLVM_RUNTIME_SOURCE)
	cp $< $@

%.wo: %.w
	w-as <$< >$@

%.wp: %.wo
	w-link -t program $^ -o $@

%.ll: %.wp
	w-to-llvm <$< >$@

%.bc: %.ll
	llvm-as <$< >$@

%.s: %.bc
	llc <$< >$@



