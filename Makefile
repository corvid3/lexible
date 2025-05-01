CXXFLAGS=-MMD @compile_flags.txt -fPIC
CFLAGS=-MMD @ccompile_flags.txt -fPIC

ifdef RELEASE
CXXFLAGS+=-O2
else
CXXFLAGS+=-Og -g
endif

LDLIBS=
LDFLAGS=

SRCS=
OBJS=$(SRCS:.cc=.o)
CSRCS=
COBJS=$(CSRCS:.c=.o)

default: test

clean:
	rm src/test.o

%.o: %.cc
	$(CXX) $(CXXFLAGS) $^ -c -o $@

%.o: %.c
	$(CC) $(CFLAGS) $^ -c -o $@

test: src/test.o
	$(CXX) $^ -o bin/test.out
