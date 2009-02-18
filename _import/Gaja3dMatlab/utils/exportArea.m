function exportArea(filename, shapefile, fieldname, standard, factor)

if(nargin > 2)
    polygons = shaperead(shapefile);

    tricount = numel(polygons);
    areamatrix = zeros(tricount, 2);
    for p=1:tricount
        areamatrix(p,1) = p;
        areamatrix(p,2) = polygons(p).(fieldname);
        if(exist('factor','var'))
           areamatrix(p,2) = areamatrix(p,2) * factor(p);
        end
    end

    areamatrix(areamatrix == 0) = standard;
else
    % workaround: two arguments means second argument is matrix
    tricount = size(shapefile,1);
    areamatrix = zeros(tricount, 2);
    areamatrix(:,1) = (1:tricount)';
    areamatrix(:,2) = shapefile;
end

%% INITIALIZE poly-file
% open for writing
fid = fopen(filename, 'w');

% write area spec
fprintf(fid,'%s\n', num2str(tricount)); % number of triangles
fprintf(fid,'%d %20.7f\n', areamatrix');

%close the handle of the file
fclose(fid);

%%
end %exportArea