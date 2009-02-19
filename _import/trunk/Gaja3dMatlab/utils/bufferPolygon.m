function [resultX, resultY] = bufferPolygon(polyX, polyY, buffer)
%% BUFFERPOLYGON Buffers a polygon with a given distance.
%
%   Uses the Java Topology Suite.
%
%   polyX, polyY - 1xN arrays with polygon coordinates
%   buffer       - a distance, positive meaning buffering to the outside
%

%% call helper class
    polyZ = zeros(size(polyX));
    resultXY = org.kalypso.gaja3d.tools.MatlabHelper.bufferPolygon(polyX, polyY, polyZ, buffer);
   
    if(isempty(resultXY))
        resultX = [];
        resultY = [];
    else
        resultX = resultXY(1,:);
        resultY = resultXY(2,:);
    end
%%
end %BUFFERPOLYGON