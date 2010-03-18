function saveMeshAsVtk(file,points,elements)
%
% This function takes as input a 2D unrestricted triangulation and export
% it to an ASCII VTK file which can be oppened with the viewer Paraview.
%
% Input :
%           "file" is the name without extension of the file (string).
%           "points" is the coordinate of the vertex of the triangulation (nx3 matrix).
%           "elements" is the list of triangles which contain indexes of
%           points (mx3 matrix).
%

nbpoint=length(points);
% pad points to be divisible by 3
if mod(nbpoint,3)==1
    points(end+1:end+2,:) = 0;
elseif mod(nbpoint,3)==2
    points(end+1,:) = 0;
end
nbpoint=length(points);

fid=fopen([file '.vtk'], 'wt');
% header 
fprintf(fid,'# vtk DataFile Version 3.0\n');
fprintf(fid,'vtk output\n');
fprintf(fid,'ASCII\n');
fprintf(fid,'DATASET POLYDATA\n');

% points
fprintf(fid,'POINTS %d float\n',nbpoint);
fprintf(fid,'%3.7f %3.7f %3.7f %3.7f %3.7f %3.7f %3.7f %3.7f %3.7f\n',[points(1:3:end-2,1:3) points(2:3:end-1,1:3) points(3:3:end,1:3)]');

% polygons
ntri=length(elements);
if(size(elements,2) == 3)
    % triangles
    fprintf(fid,'POLYGONS %d %d\n',ntri,4*ntri);
    fprintf(fid,'3 %d %d %d\n',(elements-1)');
elseif(size(elements,2) == 4)
    % triangles and quadrilaterals (mixed)
    isquad = ~isnan(elements(:,4));
    numquad = sum(isquad);
    fprintf(fid,'POLYGONS %d %d\n',ntri,4*(ntri-numquad)+5*numquad);
    fprintf(fid,'3 %d %d %d\n',(elements(~isquad,1:3)-1)');
    fprintf(fid,'4 %d %d %d %d\n',(elements(isquad,1:4)-1)');
end

fprintf(fid,'POINT_DATA %d\n',nbpoint);
numresults = size(points,2)-3;
fprintf(fid,'FIELD results %d\n',numresults);
for i=1:numresults
    fprintf(fid,'result_%d 1 %d float\n', i, nbpoint);
    fprintf(fid,'%3.7f %3.7f %3.7f\n',[points(1:3:end-2,3+i) points(2:3:end-1,3+i) points(3:3:end,3+i)]');
end

fclose(fid);
end