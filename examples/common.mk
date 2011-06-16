UNAME = $(shell uname -a)
ifeq (Darwin,$(findstring Darwin,$(UNAME)))
    OS=apple-darwin10.0.0
else
    ifeq (Linux,$(findstring Linux,$(UNAME)))
        OS=linux-gnu
    endif
endif
ifeq (i386,$(findstring i386,$(UNAME)))
    ARCH=i386
else
    ARCH=x86_64
endif

%.wp: %.wo
	w-link -t program $^ -o $@

%.ll: %.wp
	w-to-llvm <$< >$@

%.bc: %.ll
	llvm-as <$< >$@

%.s: %.bc
	llc <$< >$@
