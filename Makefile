MOD ?= dft.OCC
PREFIX ?= 0

RTL_OPT = --full-stacktrace -td build --target systemverilog --module $(MOD)
ifneq ($(PREFIX), 0)
RTL_OPT += --prefix $(PREFIX)
endif

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init cde hardfloat

idea:
	mill -i mill.idea.GenIdea/idea

rtl:
	@mkdir -p build
	mill -i xsutils.test.runMain top.TestTop $(RTL_OPT)
	@tar -zcvf build.tar.gz build

clean:
	@rm -rf build/*

sim_clean:
	@rm -rf sim/*

.PHONY:init idea clean rtl sim_clean