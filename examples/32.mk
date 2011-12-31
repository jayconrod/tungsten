# NOTE: This file is currently not used. It is provided for reference when we
# add some 32-bit examples.

LDFLAGS = -m32

runtime.ll: $(RUNTIME_PATH)/runtime32.ll
	sed -e 's/target triple = "i386-linux-gnu"/target triple = "i386-$(OS)"/' <$< >$@

tungsten.wl: ../runtime/tungsten-32.w
	w-as $< -o $@

%.w32: %.w
	echo "is64bit: false" >$@
	cat <$< >>$@

%.wo: %.w32
	w-as $< -o $@
