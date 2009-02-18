function [resultX, resultY] = simplifyLine(lineX, lineY, tolerance)
%% SIMPLIFYLINE Uses the Douglas-Peucker method to simplify a line
%
%   Uses the Java Topology Suite.
%
%   lineX, lineY - 1xN arrays with line coordinates
%   tolerance    - double that parametrizes the simplification
%
%   resultX, resultY are 1xM arrays with simplified line coordinates
%

%% call helper class
    resultXY = org.kalypso.gaja3d.tools.MatlabHelper.simplify(lineX, lineY, zeros(size(lineX)), tolerance);        
    resultX = resultXY(1,:);
    resultY = resultXY(2,:);

%%
end %SIMPLIFYLINE