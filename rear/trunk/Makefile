EXENAME = main
SOURCES = \
	Robot.cpp \
	Morduc.cpp \
	Camera.cpp \
	texture_png.cpp \
	DataLogic.cpp \
	DataManager.cpp \
	DistanceCalc.cpp \
	main.cpp

OBJECTS = $(SOURCES:.cpp=.o)
OPTIONS = -lglut -lpng
LIBRARY = -L/usr/X11R6/lib

SESSION = 8


main: $(OBJECTS)
	g++ $(LIBRARY) $(OPTIONS) -o $(EXENAME) $(OBJECTS) 

.cpp.o:
	g++ $(LIBRARY) $(OPTIONS) -c $<

# if header file changes:
# touch cpp file
# re-compile main.cpp

# .h.cpp: 
# 	touch $@
# 	touch main.cpp

clean:
	-rm *.o; rm $(EXENAME)
	-rm *.h.gch

tar:
	tar -c *.h *.cpp readme.txt -f $(EXENAME).tar

run:	main
	./${EXENAME} ${SESSION}

force:
	g++ $(SOURCES) -o $(EXENAME)