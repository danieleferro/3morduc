EXENAME = main
SOURCES = $(shell ls *.cpp)
OBJECTS = $(SOURCES:.cpp=.o)

main: $(OBJECTS)
	g++ -L/usr/X11R6/lib -lglut -o $(EXENAME) $(OBJECTS)

.cpp.o:
	g++ -L/usr/X11R6/lib -lglut -c $<

# if header file changes:
# touch cpp file
# re-compile main.cpp
.h.cpp: 
	touch $@
	touch main.cpp
clean:
	rm *.o; rm $(EXENAME)

tar:
	tar -c *.h *.cpp readme.txt -f $(EXENAME).tar

force:
	g++ $(SOURCES) -o $(EXENAME)