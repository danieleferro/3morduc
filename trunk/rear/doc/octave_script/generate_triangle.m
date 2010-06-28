clear

x = -5:0.1:15;
for i = 1 : length(x)
	z(i) = triangle(x(i), 5);
endfor

xlabel('euclidean distance value');
ylabel('score value');

title("Score function in Spacial Metric");

print("../../../report/img/spacialMetricFunc.png")

plot(x, z);
