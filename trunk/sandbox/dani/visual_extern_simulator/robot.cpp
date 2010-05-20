
#include "stdafx.h"
#include "type.h"  /// se non si mette prima dei prototipi prende quelli come definizione!
#include "robot.h"
//#include "util.h"

void paintCylinder(GLfloat radius,GLfloat height);
void paintDisk(GLfloat radius);

extern int wi;
extern int he;
extern char *dest;



void DrawPar3(GLfloat x0,GLfloat y0,GLfloat z0,GLfloat l,GLfloat h,GLfloat d )
/// xyz0 uk centro e il resto le dimensioni
{
	//glTranslatef(x0,y0,z0);
	//	glBindTexture(GL_TEXTURE_2D, texture[0]);
	glBegin(GL_QUADS);
	GLfloat direction=1.0f;

	glNormal3f(0.0f,0.0f,direction);
	glTexCoord2f(0.0f, 10.0f);glVertex3f(x0+l/2,y0+d/2,z0+h/2); //6
	glTexCoord2f(0.0f, 0.0f);glVertex3f(x0-l/2,y0+d/2,z0+h/2);
	glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0+h/2);
	glTexCoord2f(1.0f, 10.0f);glVertex3f(x0+l/2,y0-d/2,z0+h/2);
	glNormal3f(0,0,-direction);
	glTexCoord2f(0.0f, 10.0f);glVertex3f(x0+l/2,y0+d/2,z0-h/2);  //1
	glTexCoord2f(0.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0-h/2);
	glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0-h/2);
	glTexCoord2f(1.0f, 10.0f);glVertex3f(x0-l/2,y0+d/2,z0-h/2);
	glNormal3f(direction,0,0);
	glTexCoord2f(0.0f, 10.0f);glVertex3f(x0+l/2,y0+d/2,z0+h/2); //2
	glTexCoord2f(1.0f, 10.0f);glVertex3f(x0+l/2,y0-d/2,z0+h/2);
	glTexCoord2f(1.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0-h/2);
	glTexCoord2f(0.0f, 0.0f);glVertex3f(x0+l/2,y0+d/2,z0-h/2);
	glNormal3f(-direction,0,0);
	glTexCoord2f(0.0f, 10.0f);glVertex3f(x0-l/2,y0+d/2,z0+h/2); //5
	glTexCoord2f(1.0f, 10.0f);glVertex3f(x0-l/2,y0+d/2,z0-h/2);
	glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0-h/2);
	glTexCoord2f(0.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0+h/2);
	glNormal3f(0,-direction,0);
	glVertex3f(x0+l/2,y0+d/2,z0+h/2); //3
	glVertex3f(x0+l/2,y0+d/2,z0-h/2);
	glVertex3f(x0-l/2,y0+d/2,z0-h/2);
	glVertex3f(x0-l/2,y0+d/2,z0+h/2);
        /*
	  glNormal3f(0,-direction,0);
	  glVertex3f(x0+l/2,y0-d/2,z0+h/2); //4
	  glVertex3f(x0+l/2,y0-d/2,z0-h/2);
	  glVertex3f(x0-l/2,y0-d/2,z0-h/2);
	  glVertex3f(x0-l/2,y0-d/2,z0+h/2);*/
	glEnd();
}




ROBOT::ROBOT()
{
	cm=3.1412*d/n/res;               //rapporto metri / passi
	xr=4;
	yr=4;
	tetar=0;
	n = 1;                         //gear ratio
	res = 2000;                    //risoluzione encoder
	d = 0.063f*2.0f;                       //diametro della ruota
	l = 0.28f;                       //distanza tra ruote
	vlim = 7;
	wlim = 2;
	ck.Reset();
	radius=4;
	dxr=dyr=dtetar=0;
	collisions=0;
	Time==0;
}

ROBOT::ROBOT(float x1, float y2, float ang3)
{
	ROBOT();
	xr=x1;
	yr=y2;
	tetar=ang3;

}
void ROBOT::move()
{

	float dt=(float)ck.ParzialTime();
	xr+=v*cos(tetar)*dt;
	yr+=v*sin(tetar)*dt;
	tetar+=w*dt;
	Time+=dt;

	v=v-v*2*dt;  //// rallentano
	w=w-w*2*dt;

}

void ROBOT::PaintRobot2()
{
  GLfloat col_null[] = { 0.0f, 0.0f, 0.0f, 1.0f};
  
  glMaterialfv(GL_FRONT, GL_AMBIENT,col_null);
  glTranslatef(xr,0.0f,yr);
  glRotatef(-tetar/DTOR,0.0f,1.0f,0.0f);							/// angolo rotazione	
  GLfloat c,s;
  c=cos(tetar);
  s=sin(tetar);
  GLfloat light_position[] = { 0.0f, 1.3f, 0.0f, 1.0f};
  GLfloat light_direction[] = { 1.0f, 0.0f, 0.0f,1.0f};
  glLightfv(GL_LIGHT1, GL_POSITION, light_position);
  glLightf(GL_LIGHT1,GL_SPOT_CUTOFF,45.0f);
  glLightfv(GL_LIGHT1,GL_SPOT_DIRECTION,light_direction);

  glColor3f(0.2f,0.2f,0.2f);  /// grigio fumo
  glTranslatef(0.0f,0.08f,0.0f);  /// alzo da terra

  glScalef(radius,radius,radius);

  //  Disegnamo

  paintCylinder(1.0f,0.1);
  paintDisk(-1.0f);
  glTranslatef(0.0f,0.1f,0.0f);
  paintDisk(1.0f);					/// lowest disc


  glTranslatef(0.0f,0.6f,0.0f);

  paintCylinder(1.0f,0.1f);
  paintDisk(-1.0f);
  glTranslatef(0.0f,0.1f,0.0f);		/// middle disc
  paintDisk(1.0f);


  glTranslatef(.8,0,0);        ///////SICK
  glColor3f(0.5,0.5,.5);
  paintCylinder(.2,.3);
  glTranslatef(0,0.3,0);
  paintDisk(.2);

  glTranslatef(0,0.401,0);

  GLfloat col_bianco[] = { 0.2f, 0.2f, 0.2f, 1.0f}; // disco direzione
  glMaterialfv(GL_FRONT, GL_AMBIENT,col_bianco);
  paintDisk(0.1f);
  glTranslatef(0,-0.701,0);
  glMaterialfv(GL_FRONT, GL_AMBIENT,col_null);

  glTranslatef(-.8,0,0);
  /*		
		glDisable(GL_LIGHTING);
		glEnable(GL_BLEND);
		glNormal3f(cos(this->tetar),0,sin(this->tetar)); /// schermo
		glColor4f(1.0f,1.0f,1.0f,0.5f);

		//glRasterPos3f(2.0f,-2.0f,-2.0f);
		//glDrawPixels(wi,he,GL_RGB,GL_UNSIGNED_BYTE,dest);


		glBegin(GL_QUADS);
		glTexCoord2f(0.0f, 1.0f);glVertex3f(2,0.768/2,1.024/2);
		glTexCoord2f(1.0f, 01.0f);glVertex3f(2,0.768/2,-1.024/2);
		glTexCoord2f(1.0f, 0.0f);glVertex3f(2,-0.768/2,-1.024/2);
		glTexCoord2f(0.0f, 0.0f);glVertex3f(2,-0.768/2,1.024/2);
		glEnd();
		glEnable(GL_LIGHTING);
		glDisable(GL_BLEND); 

  */

  glColor3f(0.1,0.1,.1);
  glTranslatef(0,0.6f,0);
  paintCylinder(1,0.1);
  paintDisk(-1.0f);
  glTranslatef(0,0.1f,0); // fin qui 
  paintDisk(1.0f);

  glTranslatef(0,-1.5f,0);   ////// PALI

  glTranslatef(0,0,0.8);
  paintCylinder(0.1,1.5);
  glTranslatef(0,0,-1.60);
  paintCylinder(0.1,1.5);
  glTranslatef(-0.8,0,0.8);

  paintCylinder(0.1,1.5);
  glTranslatef(0.8f,0.0f,0.0f);


  glScalef(1/radius,1/radius,1/radius);


}


void ROBOT::SetW(float set)
{
	if(set>0)
	{
		if (w+set<=wlim)
			w+=set;
	}	
	else
		if (w+set>=-wlim)
			w+=set;


}

void ROBOT::SetV(float set)
{

	if(set>0)
	{
		if (v+set<=vlim)
			v+=set;
	}	
	else
		if (v+set>=-vlim)
			v+=set;

}

void ROBOT::movetank(float ul, float ur)
{
	this->SetV(cm*(ul+ur)/2);
	this->SetW(cm*(ul-ur)/l);

}



void paintDisk(GLfloat radius)
{

	glBegin(GL_POLYGON);
	float pos=-1;
	if (radius<=0) pos=pos*pos;
	glNormal3f(0,pos,0);
	//	glVertex3f(0,0,0);
	for (int kkk=0;kkk<=360;kkk++)
		glVertex3f(radius*cos(pig/180*kkk),0,radius*sin(pig/180*kkk*pos));
	glEnd();

}

void paintCylinder(GLfloat radius,GLfloat height)
{
	GLfloat c[361],s[361];
	glBegin(GL_QUAD_STRIP);

	for (int kkk=0;kkk<=360;kkk++)
	{
		c[kkk]=cos(pig/180*kkk);
		s[kkk]=sin(pig/180*kkk);
		glNormal3f(c[kkk],s[kkk],0);
		glVertex3f(radius*c[kkk],0,radius*s[kkk]);
		glVertex3f(radius*c[kkk],height,radius*s[kkk]);
	}
	for (int kkk=0;kkk<=360;kkk++)
	{
		glNormal3f(c[kkk],s[kkk],0);
		glVertex3f(radius*c[kkk],height,radius*s[kkk]);
	}

	glEnd();
}


int ROBOT::RobColl(OBJECT *coll, bool activecontrol)
{
	GLfloat tx,ty,value,mdist;			
	GLfloat col_ambtouc[]={0.4,0.2,0.2,1};
	GLfloat col_ambnorm[]={0.60,0.55,0.55,1};
	int j=0;	
	int CI=0; // collision interrupt if collision exists it will be higher than 0
	float dt=0;
	float psiob=0;
	float rex=0;
	float rey=0;

	this->vrx=0;
	this->vry=0;
	for(int i=0;i<coll->verts;i++)	// Loop Through All The Verts of coll (All Objects Have
	{												
		tx=coll->points[i].x;						
		ty=coll->points[i].y;						
		value=coll->points[i].value;
		mdist=sqrt(powf((tx - this->xr),2)+powf((ty - this->yr),2))-this->radius-value;
		if (mdist<0.0)
		{
			psiob=atan2((ty-yr),(tx-xr));
			rex+=((mdist*cos(psiob)));
			rey+=((mdist*sin(psiob))); // calcoliamo la reazione vincolare
			// coloro con draw copiato RIFARE
			glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,col_ambtouc);
			value=value * 1.05;
			if (activecontrol==true)
				DrawPar3(tx,10*value,ty,1.01f*value,1.01f*value,20*value);  // SQUARE BASE
			glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,col_ambnorm);
			if (coll->points[i].touched==0)
			{
				j++;
				coll->points[i].touched=1;
			}
			CI++;
		}
		else  
		{
			coll->points[i].touched=0;
		}
	}	

	if (CI>0)
	{
		this->vrx=rex; // i use local variable.
		this->vry=rey;

		if (activecontrol==true)
		{
			this->collisions+=j;
			this->xr+=this->vrx;  // Non � una velocit�.
			this->yr+=this->vry;  // si toglie la possibilit� di passare il muro.
		}
	}

	return CI;
}

int ROBOT::RobColl2(OBJECT *coll, bool activecontrol)
{
	GLfloat tx,ty,value,mdist;			
	GLfloat col_ambtouc[]={0.4,0.2,0.2,1};
	GLfloat col_ambnorm[]={0.60,0.55,0.55,1};
	int j=0;	
	int CI=0; // collision interrupt if collision exists it will be higher than 0
	float dt=0;
	float psiob=0;
	float rex=0;
	float rey=0;

	this->vrx=0;
	this->vry=0;
	for(int i=0;i<coll->verts;i++)	// Loop Through All The Verts of coll (All Objects Have
	{												
		tx=coll->points[i].x;						
		ty=coll->points[i].y;						
		value=coll->points[i].value;
		mdist=sqrt(powf((tx - this->xr),2)+powf((ty - this->yr),2))-this->radius-value;
		if (mdist<0.0)
		{
			psiob=atan2((ty-yr),(tx-xr));
			rex+=((mdist*cos(psiob)));
			rey+=((mdist*sin(psiob))); // calcoliamo la reazione vincolare
			// coloro con draw copiato RIFARE
			glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,col_ambtouc);
			value=value * 1.05;
			if (activecontrol==true)
				DrawPar3(tx,10*value,ty,1.01f*value,1.01f*value,20*value);  // SQUARE BASE
			glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,col_ambnorm);
			if (coll->points[i].touched==0)
			{
				j++;
				coll->points[i].touched=1;
			}
			CI++;
		}
		//else  
		//{
		//	coll->points[i].touched=0;
		//}
	}	

	if (CI>0)
	{
		this->vrx=rex; // i use local variable.
		this->vry=rey;
		this->collisions+=j;
		if (activecontrol==true)
		{
			this->xr+=this->vrx;  // Non � una velocit�.
			this->yr+=this->vry;  // si toglie la possibilit� di passare il muro.
		}
	}

	return CI;
}


bool ROBOT::LoadSPFromFile(char *string,int kk) // Load start position from file
{
	FILE *fcou;
	float x,y,t;
	fcou=fopen(string,"r");
	/* if (!fcou)
	{*/
	for (int i=0;i<kk;i++)
	{
		fscanf(fcou,"%f %f %f\n",&x,&y,&t);

		this->xr=x;
		this->yr=y;
		this->tetar=t;
	}
	fclose(fcou);
	fflush(fcou);
	return true;
	/*}
	else return false;*/
}