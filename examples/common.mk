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

CC=clang -c
AR=ar rcs

CFLAGS=-O0 -g

RUNTIME_PATH = ../llvm/runtime
RUNTIME_SOURCES = runtime.ll instanceof.c
RUNTIME_OBJECTS = $(addsuffix .o, $(basename $(RUNTIME_SOURCES)))

$(PROGRAMS): $(RUNTIME_OBJECTS)

instanceof.o: $(RUNTIME_PATH)/instanceof.c
	$(CC) $(CFLAGS) $^ -o $@

%.wp: %.wo tungsten.wl
	w-link $^ -o $@

%.ll: %.wp
	w-to-llvm $< -o $@

%.bc: %.ll
	llvm-as $< -o $@

%.s: %.bc
	llc -unwind-tables $< -o $@

%.o: %.s
	$(CC) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) $^ -o $@
