function index = inBoundary( x, y, boundary)
%% INBOUNDARY Tests if x and y are inside boundary
%
%   x,y      - the point x and y arrays to test (must be of same size)
%   boundary - a kalypso.Polygon specifying the boundary
%

%% check input
    if(~exist('x','var')|| ~exist('y','var'))
        error('Must specify x and y.');
    elseif(~exist('boundary','var') || ~isa(boundary, 'kalypso.Polygon'))
        error('Must specify a boundary of type kalypso.Polygon.');
    end
    
%% convert boundary to format for inpoly()
    polyX = boundary.getX();
    polyY = boundary.getY();
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
    
    % calculate result
    index = inpoly([x y], [polyX polyY], edges);
    
end % INBOUNDARY