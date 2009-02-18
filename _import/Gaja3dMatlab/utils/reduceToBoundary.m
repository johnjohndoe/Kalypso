function [ DTM, boundaryBuffer ] = reduceToBoundary( DTM, boundary, buffer, idxfile)
%% REDUCETOBOUNDARY Reduces a point cloud or a grid to a boundary
%
%   DTM      - either a grid struct as created by createGrid() or a Nx3 point cloud
%   boundary - geostruct for the boundary to strip to (boundary.getX(), boundary.getY())
%   buffer   - a distance, positive meaning buffering to the outside
%

%% check input
    if(~exist('DTM','var'))
        error('Must specify a DTM.');
    elseif(~exist('boundary','var'))
        error('Must specify a boundary.');
    elseif(isstruct(DTM) && exist('buffer', 'var'))
        error('Cannot handle the combination grid and buffer. TODO.');
    elseif(~exist('buffer', 'var'))
       buffer = 0;
    end

%% apply buffer
    boundaryBuffer = boundary.buffer(buffer);
    polyX = boundaryBuffer.getX();
    polyY = boundaryBuffer.getY();
    pointCount = numel(polyX);
    edges = zeros(pointCount - sum(isnan(polyX))*2 - 1, 2);
    currentEdge = 1;
    for i=1:pointCount
        if(i == pointCount)
        elseif(isnan(polyX(i)))
        elseif(isnan(polyX(i+1)))
        else
            edges(currentEdge,:) = [i i+1];
            currentEdge = currentEdge + 1;
        end
    end
        
    if(isa(DTM, 'org.kalypso.gaja3d.matlab.RectifiedGridCoverage'))
        % assume DTM is a grid as created by createGrid()
        Z = DTM.Z;
        [rows, cols] = size(Z);
        maxx = DTM.minx + DTM.dx * (cols - 1);
        maxy = DTM.miny + DTM.dy * (rows - 1);
        rangex = DTM.minx:DTM.dx:maxx;
        rangey = DTM.miny:DTM.dy:maxy;
        [gridX, gridY] = meshgrid(rangex, rangey);
        index = inpoly([gridX gridY], [polyX polyY], edges);
        %index = inpolygon(gridX, gridY, polyX, polyY);
        DTM.Z(index == 0) = NaN;
    else   
        % assume DTM is a Nx3 array (point cloud)
        index = inpoly(DTM(:,1:2), [polyX polyY], edges);
        %index = inpolygon(X, Y, boundaryBuffer.getX(), boundaryBuffer.getX());

        % strip DTM
        DTM = DTM(index,:);
    end

%% write indexfile
    if(exist('idxfile','var'))
        fid = fopen(idxfile, 'w');
        fprintf(fid,'%d\n', index);
        fclose(fid);
    end
    
%%
end % REDUCETOBOUNDARY