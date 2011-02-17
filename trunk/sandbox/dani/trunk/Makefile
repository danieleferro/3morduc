SOURCES  = \
	Robot.cpp \
	Morduc.cpp \
	Camera.cpp \
	texture_png.cpp \
	texture_jpeg.cpp \
	DataLogic.cpp \
	DataManager.cpp \
	SpacialMetricCalc.cpp \
	SweepMetricCalc.cpp \
	AnotherSweepMetricCalc.cpp \
	main.cpp

EXENAME   = REAR
OBJDIR	  = obj/
SOURCEDIR = source/
BINDIR    = ./

OBJECTS   = $(SOURCES:.cpp=.o)


CSOURCES  = $(addprefix $(SOURCEDIR), $(SOURCES))
COBJECTS  = $(addprefix $(OBJDIR), $(OBJECTS))
CEXE      = $(addprefix $(BINDIR), $(EXENAME))

CFLAGS    = -lglut -lpng
LIBRARY   = -L/usr/X11R6/lib
LIBJPEGOBJ = /usr/local/lib/libjpeg.a

CC = g++
RM = rm

SESSION   = 2
OPT_DIST  = 15

main: $(CSOURCES) $(CEXE)

$(CEXE): $(COBJECTS)
	$(CC) $(LIBRARY) $(CFLAGS) $(COBJECTS) $(LIBJPEGOBJ) -o $@ 

$(OBJDIR)%.o: $(SOURCEDIR)%.cpp
	$(CC) $(LIBRARY) $(CFLAGS) -c $< -o $@

# if header file changes:
# touch cpp file
# re-compile main.cpp

# .h.cpp: 
# 	touch $@
# 	touch main.cpp

clean:
	-$(RM) -f $(COBJECTS)
	-$(RM) -f $(CEXE)

run:	main
	./$(EXENAME) $(SESSION) $(OPT_DIST)
