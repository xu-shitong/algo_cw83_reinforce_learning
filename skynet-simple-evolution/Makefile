CC = gcc 
CFLAGS = -g -Wall -Werror -pedantic
BUILD = skynet 

all : $(BUILD)
	mv skynet train-skynet
	mv train-skynet ..

clean :
	/bin/rm -f *.o $(BUILD)

skynet: skynet.o train.o
