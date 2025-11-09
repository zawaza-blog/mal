GM2 ?= gm2
GM2FLAGS ?= -g -fiso

SRCS = step0_repl.mod step1_read_print.mod step2_eval.mod step3_env.mod step4_if_fn_do.mod step5_tco.mod step6_file.mod step7_quote.mod step8_macros.mod step9_try.mod stepA_mal.mod
BINS = $(SRCS:%.mod=%)

# Module dependencies
DYNSTRING_MODS = DynString.mod DynString.def
TYPES_MODS = Types.mod Types.def $(DYNSTRING_MODS)
READER_MODS = Reader.mod Reader.def $(TYPES_MODS)
PRINTER_MODS = Printer.mod Printer.def $(TYPES_MODS)
ENV_MODS = Env.mod Env.def $(TYPES_MODS)
CORE_MODS = Core.mod Core.def $(TYPES_MODS)

all: $(BINS)

step0_repl: step0_repl.mod
	$(GM2) $(GM2FLAGS) -o $@ $<

# Compile implementation modules first, then link with main program
step1_read_print: step1_read_print.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -o $@ step1_read_print.mod DynString.o Types.o Reader.o Printer.o

step2_eval: step2_eval.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step2_eval.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step3_env: step3_env.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step3_env.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step4_if_fn_do: step4_if_fn_do.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step4_if_fn_do.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step5_tco: step5_tco.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step5_tco.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step6_file: step6_file.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step6_file.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step7_quote: step7_quote.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step7_quote.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step8_macros: step8_macros.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step8_macros.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

step9_try: step9_try.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ step9_try.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

stepA_mal: stepA_mal.mod $(TYPES_MODS) $(READER_MODS) $(PRINTER_MODS) $(ENV_MODS) $(CORE_MODS)
	$(GM2) $(GM2FLAGS) -c DynString.mod
	$(GM2) $(GM2FLAGS) -c Types.mod
	$(GM2) $(GM2FLAGS) -c Reader.mod
	$(GM2) $(GM2FLAGS) -c Printer.mod
	$(GM2) $(GM2FLAGS) -c Env.mod
	$(GM2) $(GM2FLAGS) -c Core.mod
	$(GM2) $(GM2FLAGS) -o $@ stepA_mal.mod DynString.o Types.o Reader.o Printer.o Env.o Core.o

clean:
	rm -f $(BINS) step7_quote *.o
