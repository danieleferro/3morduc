##
## A simple background texture example
## 18.05.10
##

from OpenGL.GLUT import *
from OpenGL.GL import *
from OpenGL.GLU import *

from Image import open
import sys
import math
import time

## global variables
look_at = None
glu_perspective = None
animation_angle = 0.0
frame_rate = 20
image_id = None

light_colors = {
    "white" : (1.0, 1.0, 1.0, 1.0),
    "red"   : (1.0, 0.0, 0.0, 1.0),
    "green" : (0.0, 1.0, 0.0, 1.0),
    "blue"  : (0.0, 0.0, 1.0, 1.0)
    }

def load_image ( u_image_name = 'nehe.bmp' ):
    """
    Loads an image file, then converts it into a texture object
    and store it.
    """
    global image_id
    
    im = open (u_image_name)

    try:
        ## get image meta-data (dimensions) and data
        width,height, image = im.size[0], im.size[1], im.tostring("raw", "RGBA", 0, -1)
    except SystemError:
        ## has no alpha channel, synthesize one, see the
        ## texture module for more realistic handling
        width,height, image = im.size[0], im.size[1], im.tostring("raw", "RGBX", 0, -1)
                
    ## generate a texture ID
    ID = glGenTextures(1)

    ## make it current
    glBindTexture(GL_TEXTURE_2D, ID)

    ## specify how to align every pixel row in memory
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1)

    ## copy the texture into the current texture ID
    gluBuild2DMipmaps( GL_TEXTURE_2D, 3, width, height,
                       GL_RGBA, GL_UNSIGNED_BYTE, image )

    ## the following code won't work unless the image is square
    #     glTexImage2D(GL_TEXTURE_2D, 0, 3, width, height, 0, 
    #                  GL_RGBA, GL_UNSIGNED_BYTE, image)

    ## save the ID for use
    image_id = ID


def set_light_color ( u_color ):
    """
    Set light color to 'white', 'red', 'green' or 'blue'.
    """
    if light_colors.has_key( u_color ):
        c = light_colors[ u_color ]
        glLightfv( GL_LIGHT0, GL_AMBIENT, c ) # set ambient light parameters
        glLightfv( GL_LIGHT0, GL_DIFFUSE, c ) # set diffused light parameters
        glLightfv( GL_LIGHT0, GL_SPECULAR, c ) # set reflected light parameters


def render ():
    """
    Invokes GL and GLUT functions to actually display objects
    """
    boxSize = 50 # size of cube height, width and depth = 50mm
    glViewport( 0, 0,
		glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT ))

    g = glu_perspective 
    l = look_at

    if g is None or l is None:
        raise ("ERROR: perspective parameters not set!")

    ## set up projection matrix
    glMatrixMode ( GL_PROJECTION )
    glLoadIdentity ()
    gluPerspective ( g[0], g[1], g[2], g[3] )

    ## set up modelview matrix
    glMatrixMode ( GL_MODELVIEW )
    glLoadIdentity ()
    gluLookAt ( l[0], l[1], l[2], l[3],
                l[4], l[5], l[6], l[7],
                l[8] )

    ## draw a cube
    glPushMatrix()
    glTranslatef( 0.0, 0.0, -900 )
    glRotatef( animation_angle, 0.2, 0.7, 0.3 ) # rotates the cube
    glutSolidCube( boxSize )
    glPopMatrix()

    ## reset porjection and modelview matrix
    glMatrixMode ( GL_PROJECTION )
    glLoadIdentity ()
    glMatrixMode ( GL_MODELVIEW )
    glLoadIdentity ()

    ## map texture
    if image_id is not None:
        glEnable ( GL_TEXTURE_2D )
        glTexParameterf ( GL_TEXTURE_2D, 
                          GL_TEXTURE_MAG_FILTER, GL_NEAREST ) ## tell how to magnify a texture image

        glTexParameterf ( GL_TEXTURE_2D, 
                          GL_TEXTURE_MIN_FILTER, GL_NEAREST ) ## tell how to reduce in size a texture image

        glTexEnvf ( GL_TEXTURE_ENV, 
                    GL_TEXTURE_ENV_MODE, GL_DECAL ) ## tell how to apply texture values to fragments

        glBindTexture ( GL_TEXTURE_2D, image_id )
        
        ## map the texture
        glBegin( GL_QUADS )
        glTexCoord2f(0.0, 0.0); glVertex2f(-1.0, -1.0);
        glTexCoord2f(1.0, 0.0); glVertex2f(1.0, -1.0);
        glTexCoord2f(1.0, 1.0); glVertex2f(1.0, 1.0);
        glTexCoord2f(0.0, 1.0); glVertex2f(-1.0, 1.0);
        glEnd()
        glDisable ( GL_TEXTURE_2D )

def step ():
    """
    Update animation parameters
    """
    global animation_angle
    global frame_rate
    animation_angle += 2
    while animation_angle > 360:
        animation_angle -= 360
    time.sleep( 1 / float ( frame_rate ))
    glutPostRedisplay( ) # marks the current windows to be redisplayed

def draw ():
    """
    Glut display function
    """
    glDrawBuffer ( GL_BACK )
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
    set_light_color ( "white" )
    render ()
    glutSwapBuffers ( )

def init():
    """
    Initializes the application.
    """
    global glu_perspective 
    global look_at

    ## specifies the values of R,G,B,Alpha to set when
    ## glClearColor is invoked
    glClearColor(0,0,0,0)
    
    ## enable depth test
    glEnable ( GL_DEPTH_TEST )

    ## enable and set lighting
    glEnable ( GL_LIGHT0 )
    glLightModeli ( GL_LIGHT_MODEL_TWO_SIDE, 0 )
    glLightfv ( GL_LIGHT0, GL_POSITION, [4, 4, 4, 4] )
    glLightfv ( GL_LIGHT0, GL_AMBIENT, [0.8, 0.8, 0.8, 1] )
    glLightfv ( GL_LIGHT0, GL_DIFFUSE, [1, 1, 1, 1] )
    glLightfv ( GL_LIGHT0, GL_SPECULAR, [1, 1, 1, 1] )
    
    ## calculate perspective
    w = 518.4 # Physical display dimensions in mm (Width)
    h = 324.0 # (Height)

    # w = glutGet ( GLUT_WINDOW_WIDTH ) # get window width
    # h = glutGet ( GLUT_WINDOW_HEIGHT ) # get window height
    Z = 1000.0 # Distance in the scene from the camera to the display plane.
    
    Near = 800.0  # Distance in the scene from the camera to the near plane.
    Far = 1200.0 # Distance in the scene from the camera to the far plane.

    half_fov = math.atan( (h/2.0) / Z )
    fov = math.degrees( 2.0 * half_fov ) # field of view (fov) in y (vertical axis) direction.		

    glu_perspective = ( fov, w/h, Near, Far ) # Parameters for gluPerspective
    look_at = ( 0, 0, 0, 0, 0, -Z, 0, 1, 0 ) # Lookat points for right eye/camera

    ## load texture image
    load_image ()    
    

glutInit ( sys.argv )
glutInitDisplayMode ( GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH )
glutInitWindowSize ( 800, 600 )
glutInitWindowPosition ( 100, 100 )
glutCreateWindow ( sys.argv[0] )
init ()
glutDisplayFunc ( draw )
glutIdleFunc ( step )
glutMainLoop ()

