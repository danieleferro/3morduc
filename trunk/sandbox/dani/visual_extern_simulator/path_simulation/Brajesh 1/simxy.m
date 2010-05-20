function simxy(num1)
wc=imread('../Data/map.jpg');
imagesc(wc);
hold on;
a=load(['path_',num2str(num1),'.txt']);
x=a(:,1) ; 
y=a(:,2);
t=a(:,3);
vx=a(:,6);
vy=a(:,7);
plot(x,y,'.');
plot(x,y);
quiver(x,y,vx,vy,'r'); %% disegna vettore spostamento 
mod=sqrt(vx.^2+vy.^2);
maxmod=max(mod);
quiver(x,y,maxmod*cos(t)/10,maxmod*sin(t)/10,'g'); %% disegna vettore orientamento robot