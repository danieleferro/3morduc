clear

number = input("Number of session: ", "s");

path = [ "../../log/log_", number, "/data_", number, ".txt"];

S = load("-ascii", path);

S = S(:, 1:2);




path_output = ["../../../report/img/path_session_", number, ".png"];

% clear previous plot
clf();

hold on;
xlabel('Y value');
ylabel('X value');
title([ "Path session # ", number ]);
axis(  [0, 140, 0, 80 ]);
plot( S(:, 1), S(:, 2), 'o');


print(path_output);


disp("File saved in: ");
disp(path_output);
