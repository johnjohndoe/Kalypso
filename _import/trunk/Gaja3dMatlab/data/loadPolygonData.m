function [ boundaries ] = loadPolygonData( file, hasheader, params  )
%% LOADPOLYGONDATA Loads polygon data from shape (.shp) or text (.txt, .xyz) file
%               distinguish by type and ascii delimiter
%
%   file      - valid filename
%   (The following parameters are only used when loading a text file)
%   hasheader - if 1, also read first line of file (defaults to 1 = has header)
%   params    - cell array of strings (param/value pairs) as described for
%               textscan
%

%% check input
    if(~exist('file','var'))
        error('Must specify a filename.');
    elseif(~exist(file,'file'))
        error('%s is not a valid filename or file does not exist.', file);
    end
    if(~exist('hasheader','var'))
        hasheader = 1;
    end
    if(~exist('params','var'))
        params = cell(0);
    end

%% switch file extension
    SHPEXT = '.shp';
    TXTEXT = '.txt';
    XYZEXT = '.xyz';
    [pathstr, name, ext] = fileparts(file);
    switch(ext)
        case SHPEXT
            %read shapefile information
            info = shapeinfo(file);
            if(~(strncmp(info.ShapeType, 'Polygon',7)))
                error('Shapefile %s does not have polygon geometry.', file);
            end                        
            %read shapefile
            boundaries = shaperead(file);
        case {TXTEXT, XYZEXT}
            % load the points and interpret as consecutive line segments
            boundaryMatrix = loadPointData(file, hasheader, params);            
            % convert to geostruct with one entry
            ID = 1;
            X = boundaryMatrix(:,1);
            Y = boundaryMatrix(:,2);
            B = [min(X), min(Y); max(X), max(Y)];
            boundaries = struct('Geometry', 'Polygon', 'BoundingBox', B, 'X', X, 'Y', Y, 'ID', ID);
    end
%%
end %LOADPOLYGONDATA
