function lu=simxydaa(cos,cod)
wc=imread('../Data/map.jpg');
imagesc(wc);
hold on;
lu=[];
% for j=cos,cod    
    for j=cos:cod
        a=load(['../path_simulation/path_',num2str(j),'.txt']);
        plot(a(:,1),a(:,2),'.');
        lu=[lu,size(a,1)];
%%        pause;
    end
% end