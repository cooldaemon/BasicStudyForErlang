EMULATOR=beam
EBIN=$(ROOT)/ebin
INCLUDE=$(ROOT)/include

SOURCES = $(wildcard *.erl)
HEADERS = $(wildcard $(INCLUDE)/include/*.hrl)
OBJECTS = $(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

COMPILE_FLAGS += \
	+warn_unused_vars +nowarn_shadow_vars +warn_unused_import

ifndef debug_info
	COMPILE_FLAGS += +debug_info
endif

$(EBIN)/%.$(EMULATOR): %.erl
	erlc -pa $(EBIN) -W $(COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

all: $(OBJECTS)

$(OBJECTS): $(HEADERS)

clean:	
	rm -rf $(EBIN)/*.$(EMULATOR) $(EBIN)/erl_crash.dump


