COMPILE_FLAGS = \
	+warn_unused_vars +nowarn_shadow_vars +warn_unused_import \
	+debug_info

SOURCES = $(wildcard *.erl)
OBJECTS = $(SOURCES:%.erl=%.beam)

%.beam:%.erl
	erlc -pa . -W $(COMPILE_FLAGS) $<

all: $(OBJECTS)

clean:	
	rm -rf *.beam erl_crash.dump

