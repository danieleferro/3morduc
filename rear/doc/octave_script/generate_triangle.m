clear

x = -5:0.1:15;
for i = 1 : length(x)
	z(i) = triangle(x(i), 5);
endfor

% clear previous plot
clf();

hold on;
xlabel('euclidean distance value');
ylabel('score value');
title("Score function in Spacial Metric");
path_output = "../../../report/img/spacialMetricFunc.png";
plot(x, z);

print(path_output)

disp("File saved in: ");
disp(path_output);

