function [gridx, gridy, refmat] = createGrid(points, dx, dy)
%% CREATEGRID Creates a grid with given resolution for a digital terrain model.
%
%   points      - Nx3 array with point X,Y,Z-coordinates
%   dx, dy      - grid resolution (optional, default=1)
%
% The grid can be visualized with the Mapping Toolbox as follows:
% >> mapshow(grid.Z, grid.refmat, 'DisplayType', 'surface');
%

    if(~exist('points','var') || size(points, 2) < 2)
        error('Must specify at least nx2 elevation data with XY-coordinates.');
    end
    if(~exist('dx', 'var') || isempty(dx))
        dx = 1;
    end
    if(~exist('dy', 'var') || isempty(dy))
        dy = 1;
    end
   
    % extract columns
    X = points(:,1);
    Y = points(:,2);

    % create referencing matrix
    minx = min(X);
    miny = min(Y);
    refmat = constructRefMat(minx, miny, dx, dy);
    
    % mesh grid
    maxx = max(X);
    maxy = max(Y);
    rangex = minx:dx:maxx;
    rangey = miny:dy:maxy;
    [gridx, gridy] = meshgrid(rangex, rangey);
    
end %CREATEGRID