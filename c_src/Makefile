# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

C_SRC_DIR = $(CURDIR)

EXML_NIF = $(CURDIR)/../priv/exml_nif.so

C_SRC_OUTPUT = $(EXML_NIF)

all: $(C_SRC_OUTPUT)

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	# It's a bit non-standard for a Makefile but OSX users will appreciate
	CXXFLAGS += -arch x86_64 -finline-functions
	LDFLAGS += -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CXXFLAGS ?= -I/usr/local/include
	LDFLAGS ?= -L/usr/local/lib
else ifeq ($(UNAME_SYS), Linux)
	CXXFLAGS ?= -finline-functions
endif

CXXFLAGS += -std=c++11 -Wall -fPIC -O3 -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(EXML_NIF): exml.o
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CXX) $^ $(LDFLAGS) $(LDLIBS) -o $@

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
