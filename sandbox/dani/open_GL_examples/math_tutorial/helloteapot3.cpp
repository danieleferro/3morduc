#include <GL/gl.h>
#include <GL/glut.h>

void display () {

    /* clear window */
    glClear(GL_COLOR_BUFFER_BIT);

    /* future matrix manipulations should affect the modelview matrix */
    glMatrixMode(GL_MODELVIEW);

    /* draw scene */
    glutSolidTeapot(.5);

    /* flush drawing routines to the window */
    glFlush();

}

void reshape ( int width, int height ) {

    /* define the viewport transformation */
    glViewport(0,0,width,height);

}

int main ( int argc, char * argv[] ) {

    /* initialize GLUT, using any commandline parameters passed to the 
       program */
    glutInit(&argc,argv);

    /* setup the size, position, and display mode for new windows */
    glutInitWindowSize(500,500);
    glutInitWindowPosition(0,0);
    glutInitDisplayMode(GLUT_RGB);

    /* create and set up a window */
    glutCreateWindow("hello, teapot!");
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);

    /* define the projection transformation
       before any drawing occurs
       
    */

    // subsequent commands will affect the projection transformation stack
    glMatrixMode(GL_PROJECTION);

    // reset the matrix at the bottom of the stack with an identity
    glLoadIdentity();


    // glOrtho describes a transformation that produces a parallel projection.
    // The current matrix (see glMatrixMode) is multiplied by this matrix
    // and the result replaces the current matrix
    glOrtho(-2.0, 2.0, -2.0, 2.0, -2.0, -2.0);

    /* tell GLUT to wait for events */
    glutMainLoop();
}
