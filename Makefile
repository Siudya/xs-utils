MOD ?= dft.OCC

RTL_OPT = --full-stacktrace -td build/rtl --target systemverilog --split-verilog --module $(MOD)
ifdef PREFIX
RTL_OPT += --prefix $(PREFIX)
endif

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init cde hardfloat

idea:
	mill -i mill.idea.GenIdea/idea

rtl:
	@mkdir -p build/rtl
	@mkdir -p build/macro
	mill -i xsutils.test.runMain top.TestTop $(RTL_OPT)
	@mv build/rtl/$(PREFIX)sram_array_* build/macro/
	@mv build/rtl/$(PREFIX)ClockGate* build/macro/
	@rm build/rtl/*.f
	@tar -zcvf build.tar.gz build

clean:
	@rm -rf build/*

sim_clean:
	@rm -rf sim/*

.PHONY:init idea clean rtl sim_clean