function [ resultX, resultY, resultZ ] = clipLine( lineX, lineY, polyX, polyY)
%% CLIPLINE Clips a line with a polygon and returns the resulting lines
%
%   Uses the Java Topology Suite.
%
%   lineX, lineY are 1xN arrays with line coordinates
%   linesX, linesY are 1xM arrays with NaN-separated line coordinates
%

%% call helper class
    lineZ = zeros(size(lineX));
    polyZ = zeros(size(polyX));
    resultXYZ = org.kalypso.gaja3d.tools.MatlabHelper.clipLine(lineX, lineY, lineZ, polyX, polyY, polyZ);

    if(isempty(resultXYZ))
        resultX = [];
        resultY = [];
        resultZ = [];
    else
        resultX = resultXYZ(1,:);
        resultY = resultXYZ(2,:);
        resultZ = resultXYZ(3,:);
    end
%%
end %CLIPLINE