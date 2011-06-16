LLVM_RUNTIME_SOURCE = ../llvm/runtime/runtime64.ll
RUNTIME_NAME = $(notdir $(LLVM_RUNTIME_SOURCE))

LDFLAGS = -m64

$(PROGRAMS): $(RUNTIME_NAME:%.ll=%.s)

$(RUNTIME_NAME): $(LLVM_RUNTIME_SOURCE)
	sed -e 's/target triple = "x86_64-linux-gnu"/target triple = "x86_64-$(OS)"/' <$< >$@

%.wo: %.w
	w-as <$< >$@
