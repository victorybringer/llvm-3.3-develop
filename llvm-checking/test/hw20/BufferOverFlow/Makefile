CC = gcc
CXX = g++


INCLUDE=
SOURCES := $(wildcard *.cpp) $(wildcard *.c)

OBJS = $(patsubst %.c,%.o,$(SOURCES))
OBJS:= $(patsubst %.cpp,%.o,$(OBJS))

all:$(OBJS) 

%.o:%.c 
	$(CC) $(INCLUDE) -c $< 

%.o:%.cpp 
	$(CXX) $(INCLUDE) -c $<
clean:
	rm -rf *.o