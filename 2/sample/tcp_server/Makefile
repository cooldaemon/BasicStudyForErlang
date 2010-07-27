ifndef ROOT
	ROOT=$(shell pwd)
endif

all: subdirs

subdirs:
	cd src; ROOT=$(ROOT) make

clean:	
	rm -rf *.beam erl_crash.dump
	cd src; ROOT=$(ROOT) make clean

