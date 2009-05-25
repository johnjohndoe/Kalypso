function [Z,R] = readAsciiGrid( filename )
%READASCIIGRID Read gridded data set in ASCII Grid Format
%
%   [Z, R] = READASCIIGRID(FILENAME) reads a grid from a file in Arc ASCII
%   Grid format.  Z is a 2D array containing the data values.  R is a
%   referencing matrix (see CREATEREFMAT).  NaN is assigned to elements of Z
%   corresponding to null data values in the grid file.
%

%% Read file.
fid = fopen(filename,'r');

% Read the 6-line header.
[ncols, nrows, xllcorner, yllcorner, cellsize, nodata] = readHeader(fid);

% Read the matrix of data rows backwards
Z = zeros(nrows,ncols);
for i=nrows:-1:1
    % read row i
    Zrow = fscanf(fid,'%g',ncols);
    % make nodata NaN
    Zrow(Zrow == nodata) = NaN;
    % integrate in Z backwards
    Z(nrows-i+1,:) = Zrow;
end
fclose(fid);

%% Construct the referencing matrix.
R = constructRefMat(xllcorner + cellsize/2,...
               yllcorner + (nrows - 1/2) * cellsize,...
               cellsize, cellsize);
end % READASCIIGRID

function [ncols, nrows, xllcorner, yllcorner,...
                        cellsize, nodata] = readHeader(fid)
itemNames = cell(1,6);
value = cell(1,6);
for k = 1:6
    line = fgetl(fid);
    [token,rem] = strtok(line);
    itemNames{k} = token;
    value{k} = str2double(rem);
end

ncols     = value{1};
nrows     = value{2};
xllcorner = value{3};
yllcorner = value{4};
cellsize  = value{5};
nodata    = value{6};
end % readHeader
