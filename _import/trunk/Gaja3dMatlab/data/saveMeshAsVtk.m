function saveMeshAsVtk(file,points,elements)
%
% This function saves a list of points and faces 
% to an ASCII VTK file that can be opened with Paraview.
%

fid = fopen([file '.vtk'], 'wt');

% header 
fprintf(fid,'# vtk DataFile Version 3.0\n');
fprintf(fid,'vtk output\n');
fprintf(fid,'ASCII\n');
fprintf(fid,'DATASET POLYDATA\n');

% points
nbpoint=length(points);
fprintf(fid,'POINTS %d float\n',nbpoint);
fprintf(fid,'%3.7f %3.7f %3.7f\n',points(:,1:3)');

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
    fprintf(fid,'%3.7f\n',points(:,3+i)');
end

fclose(fid);
end