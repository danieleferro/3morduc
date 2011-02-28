SOURCES  = \
	Robot.cpp \
	Morduc.cpp \
	Camera.cpp \
	texture_png.cpp \
	texture_jpeg.cpp \
	jpeg_library.cpp \
	opengl_library.cpp \
	HTTPFunctor.cpp \
	DataLogicLogSimulator.cpp \
	DataLogicLogMorduc.cpp \
	DataLogicMorduc.cpp \
	DataManager.cpp \
	SpacialMetricCalc.cpp \
	SweepMetricCalc.cpp \
	AnotherSweepMetricCalc.cpp \
	input_check.cpp \
	main.cpp

EXENAME   = REAR
OBJDIR	  = obj/
SOURCEDIR = source/
BINDIR    = ./

OBJECTS   = $(SOURCES:.cpp=.o)


CSOURCES  = $(addprefix $(SOURCEDIR), $(SOURCES))
COBJECTS  = $(addprefix $(OBJDIR), $(OBJECTS))
CEXE      = $(addprefix $(BINDIR), $(EXENAME))

CFLAGS    = -lglut -lpng -lGLU -lcurlpp -ljpeg
LIBRARY   = -L/usr/X11R6/lib

CC = g++
RM = rm

main: $(CSOURCES) $(CEXE)

$(CEXE): $(COBJECTS)
	$(CC) $(LIBRARY) $(CFLAGS) $(COBJECTS) -o $@ 

$(OBJDIR)%.o: $(SOURCEDIR)%.cpp
	$(CC) $(LIBRARY) $(CFLAGS) -c $< -o $@

clean:
	-$(RM) -f $(COBJECTS)
	-$(RM) -f $(CEXE)

# three different run commands
online:	main
	./$(EXENAME) -dl morduc 192.168.1.2 -r 5 -is asweep 25

log:	main
	./$(EXENAME) -dl logmorduc 4 -r 5 -is asweep 25

logsim:	main
	./$(EXENAME) -dl logsimul 2 -r 4 -is asweep 25