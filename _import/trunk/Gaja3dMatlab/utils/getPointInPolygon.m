function [px, py] = getPointInPolygon(polyX, polyY)
%% GETPOINTINPOLYGON Returns a point inside a polygon. Handles holes.
%
%   Uses the Java Topology Suite.
%
%   polyX, polyY are 1xN arrays with polygon coordinates
%

%% call helper class
    polyZ = zeros(size(polyX));
    resultXY = org.kalypso.gaja3d.tools.MatlabHelper.getPointInPolygon(polyX, polyY, polyZ);
    px = resultXY(1);
    py = resultXY(2);

%%
end %GETPOINTINPOLYGON