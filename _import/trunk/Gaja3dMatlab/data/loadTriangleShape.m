function [elements, points] = loadTriangleShape( filename )
%LOADTRIANGLESHAPE Load polygon data from a Polygon or PolygonZ shape file
    [pathstr,name,ext] = fileparts(filename);

    shapefileExtensions = {'.shp','.shx','.dbf'};
    if isempty(ext)
        basename = filename;
    elseif any(strcmpi(ext,shapefileExtensions))
        basename = fullfile(pathstr,name);
    else
        basename = filename;
    end

    filename = [basename '.shp'];
    
    try
        shpFileId = fopen(filename);
        if (shpFileId == -1)
            error('Could not open file %s.', filename);
        end;

        fileHeaderLength = 100; %for shape files in general

        fseek(shpFileId,24,'bof'); %file length
        fileLength = 2 * fread(shpFileId,1,'uint32','ieee-be');

        fseek(shpFileId,32,'bof'); %shape type code
        headerTypeCode = fread(shpFileId,1,'uint32','ieee-le');

        switch(headerTypeCode)
            case 5 % Polygon
                loadPointMethod = @readPolygons;
                recordLength = 120;
            case 15 % PolygonZ
                loadPointMethod = @readPolygonsZ;
                recordLength = 168;
            otherwise
                error('Shape geometry type neither Polygon nor PolygonZ.');
        end

        recordCount = (fileLength - fileHeaderLength) / recordLength; %calculate from file length
        if(floor(recordCount) ~= recordCount)
            error('File %s apparently contains polygons other than triangles.', filename);
        end

        recordsStart = fileHeaderLength; %beginning of records

        fseek(shpFileId, recordsStart + 56,'bof'); % start reading first x value
        points = loadPointMethod();
        [b, m, n] = unique(points(:,1:2), 'rows');
        points = points(m, :);
        elements = reshape(n, 4, recordCount)';
        elements = elements(:,[1 3 2]); % discard double point info
    catch err
        fclose(shpFileId);
        rethrow(err);
    end
    fclose(shpFileId);

    function points = readPolygonsZ()
        %% read all PolygonZs from file
        polygons.Geometry = 'Polygon';
        points = zeros(4*recordCount, 3);        
        for r=0:(recordCount-1)
            pRange = (r*4+1):(r*4+4);
            xy = fread(shpFileId, [2 4], '8*double', 'ieee-le'); %read four coordinate pairs
            points(pRange, 1:2) = xy';
            points(pRange, 3) = fread(shpFileId, [4 1], '4*double', 72, 'ieee-le'); %read four Z coordinates, skip to next
        end
    end

    function points = readPolygons()
        %% read all Polygons from file
        points = fread(shpFileId,[2 recordCount*4],'8*double',56,'ieee-le')'; %read 4 points per polygon, skip 56 bytes to next
    end

end %LOADTRIANGLESHAPE

