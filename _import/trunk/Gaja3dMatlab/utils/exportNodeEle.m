function exportNodeEle( filename, shapefile, interpZ )

polygons = loadTriangleShape(shapefile);
tricount = numel(polygons);

if(exist('interpZ','var') && interpZ)
   if(~isfield(polygons,'Z'))
       error('No Z information found.');
   end
   nodes = zeros(0, 3); 
else
   nodes = zeros(0, 2);
end

nodecount = 1;
for p=1:tricount
    nodecount3 = nodecount + 3;
    X = polygons(p).X(1:4);
    Y = polygons(p).Y(1:4);    
    nodes(nodecount:nodecount3, 1) = X;
    nodes(nodecount:nodecount3, 2) = Y;
    if(size(nodes,2) == 3)
        Z = polygons(p).Z(1:4);
        nodes(nodecount:nodecount3, 3) = Z;
    end
    nodecount = nodecount3 + 1;
end

nodes = unique(nodes, 'rows');

exportNodes(nodes, filename);

tris = zeros(tricount, 4);
for p=1:tricount
    [X, Y] = poly2ccw(polygons(p).X(1:4), polygons(p).Y(1:4));
    index1 = find(nodes(:,2) == X(1) & nodes(:,3) == Y(1),1);
    index2 = find(nodes(:,2) == X(2) & nodes(:,3) == Y(2),1);
    index3 = find(nodes(:,2) == X(3) & nodes(:,3) == Y(3),1);
    tris(p, :) = [p index1 index2 index3];
end

%% INITIALIZE ele-file
% open for writing
fid = fopen([filename '.ele'], 'w');

% write elements spec
fprintf(fid,'%d 3 0\n', tricount); % number of triangles, no attributes supported
fprintf(fid,'%d %d %d %d\n', tris');

%close the handle of the file
fclose(fid);

end
