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

% Read the matrix of data values, putting the k-th row in the data
% file into the k-th column of matrix Z.  Close file -- nothing left to
% read after this.
Z = fscanf(fid,'%g',[ncols,nrows]);
fclose(fid);

% Replace each no-data value with NaN.
Z(Z == nodata) = NaN;
  
% Orient the data so that rows are parallel to the x-axis and columns
% are parallel to the y-axis (for compatibility with MATLAT functions
% like SURF and MESH).
Z = Z';

%% Construct the referencing matrix.
R = constructRefMat(xllcorner + cellsize/2,...
               yllcorner + (nrows - 1/2) * cellsize,...
               cellsize, -cellsize);
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
