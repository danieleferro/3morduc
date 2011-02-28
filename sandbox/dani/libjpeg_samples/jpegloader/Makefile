CC = g++

jpegloader: jpegloader.o main.o
	$(CC) -lglut -ljpeg -o jpegloader jpegloader.o main.o 

jpegloader.o: jpegloader.cpp
	$(CC) -lglut -c jpegloader.cpp -o jpegloader.o 

main.o: main.cpp
	$(CC) -lglut -c main.cpp -o main.o

clean:
	rm -rf *.o jpegloader

run:	jpegloader
	./jpegloader