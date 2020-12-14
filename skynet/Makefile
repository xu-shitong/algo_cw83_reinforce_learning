CC = gcc 
BUILD = skynet
IncludePath = $(PWD)
CFLAGS = -Wall -g -D_DEFAULT_SOURCE -std=c99 -pedantic $(IncludePath)

all : $(BUILD)
	mv skynet train-skynet
	mv train-skynet ..

clean :
	/bin/rm -f *.o $(BUILD)

skynet: skynet.o train.o

skynet.o: skynet.c
	$(CC) $(CFLAGS) -c -o $@ $<

train.o: train.c
	$(CC) $(CFLAGS) -c -o $@ $<