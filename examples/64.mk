LDFLAGS = -m64

runtime.ll: $(RUNTIME_PATH)/runtime64.ll
	sed -e 's/target triple = "x86_64-linux-gnu"/target triple = "x86_64-$(OS)"/' <$< >$@

tungsten.wl: ../runtime/tungsten-64.w
	w-as $< -o $@

%.wo: %.w
	w-as $< -o $@
