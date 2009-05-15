function [gridx, gridy, refmat] = createGrid(X, Y, dx, dy)
%% CREATEGRID Creates a grid with given resolution for a digital terrain model.
%
%   X, Y     - X and Y sample data (used to determine minimum and maximum
%              grid coordinates
%   dx, dy   - grid resolution (optional, defaults to 1)
%

    if(~exist('X','var') || ~exist('Y','var'))
        error('Must specify X and Y sample data.');
    end
    if(~exist('dx', 'var') || isempty(dx))
        dx = 1;
    end
    if(~exist('dy', 'var') || isempty(dy))
        dy = 1;
    end

    % create referencing matrix
    minx = min(X) - (dx / 2);
    miny = min(Y) - (dy / 2);
    maxx = max(X) - (dx / 2);
    maxy = max(Y) - (dy / 2);
    refmat = constructRefMat(minx, miny, dx, dy);
    
    % mesh grid
    rangex = minx:dx:maxx;
    rangey = miny:dy:maxy;
    [gridx, gridy] = meshgrid(rangex, rangey);
    
end %CREATEGRID