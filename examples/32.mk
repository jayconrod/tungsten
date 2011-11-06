# NOTE: This file is currently not used. It is provided for reference when we
# add some 32-bit examples.

LLVM_RUNTIME_SOURCE = ../llvm/runtime/runtime32.ll
RUNTIME_NAME = $(notdir $(LLVM_RUNTIME_SOURCE))

LDFLAGS = -m32

$(PROGRAMS): $(RUNTIME_NAME:%.ll=%.s)

$(RUNTIME_NAME): $(LLVM_RUNTIME_SOURCE)
	sed -e 's/target triple = "i386-linux-gnu"/target triple = "i386-$(OS)"/' <$< >$@

tungsten.wl: ../runtime/tungsten-32.w
	w-as $< -o $@

%.w32: %.w
	echo "is64bit: false" >$@
	cat <$< >>$@

%.wo: %.w32
	w-as $< -o $@
