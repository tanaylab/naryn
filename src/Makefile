CC=g++

CFLAGS=-O3 -DNDEBUG -fpic -I$(R_INCLUDE_DIR) -std=c++11 -Wall -pedantic -fdiagnostics-color=always

TGT=naryn.so
SRCS=$(wildcard *.cpp)
OBJS=$(SRCS:.cpp=.o)
DEPS=$(wildcard *.d)

all: $(TGT)

%.o: %.cpp
	$(CC) $(CFLAGS) -c -o $@ $< -MP -MMD

$(TGT): $(OBJS)
	$(CC) -L$(R_HOME)/lib -lpthread -shared -o $@ $^ $(USR_LINK_FLAGS)

clean:
	rm -f $(OBJS) $(DEPS)

-include $(DEPS)

