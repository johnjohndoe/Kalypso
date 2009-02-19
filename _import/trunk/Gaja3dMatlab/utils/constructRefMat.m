function [ W ] = constructRefMat( x11, y11, dx, dy )
    W = [dx 0 x11; 0 dy y11];
    C = [0  1  -1; 1  0  -1; 0  0   1];
    W = (W * C)';
end
