function exportPoly(filename, boundary, breaklines, holes)
%% Exports the .poly file for the Triangle mesh generator
%
% Triangle can be found under: http://www.cs.cmu.edu/~quake/triangle.html
%
%   filename   - where to save, should have the extension .poly
%   boundary   - geostruct for the outer boundary (boundary.X, boundary.Y)
%   breaklines - geostruct for the breaklines (breaklines.X, breaklines.Y)
%
% This function exports the nodes and segments from the outer boundary and the
% breaklines into a .poly file suitable for Triangle.
% 
% TODO: write better code, use inner function for assembling matrices
%

%% check if boundary is NaN-terminated and remove if necessary
% TODO: also do this for every breakline and hole

%% create matrices for nodes and segments
    segmentsMatrixFinal = zeros(0,2);
    nodeCount = 1; 
    if(exist('boundary', 'var') && ~isempty(boundary))
        if(isnan(boundary.X(end)))
            boundary.X = boundary.X(1:end-1);
            boundary.Y = boundary.Y(1:end-1);
        end

        % boundary    
        nodesMatrixFinal = [boundary.X' boundary.Y'];   
        for n = 1:(numel(boundary.X)-1)
            node1 = nodeCount;
            node2 = nodeCount + 1;
            segmentsMatrixFinal = [segmentsMatrixFinal; node1 node2];
            nodeCount = node2;
        end    
        nodeCount = nodeCount + 1;
    else
        nodesMatrixFinal = zeros(0,2);
    end
    % breaklines  
    if(exist('breaklines', 'var'))
        for i=1:numel(breaklines)
            linei = breaklines(i);        
            lineiXj = linei.X;
            lineiYj = linei.Y;
            nodesMatrixFinal = [nodesMatrixFinal; lineiXj' lineiYj'];% lineiZj'];

            for n = 1:(size(lineiXj,2)-1)
                node1 = nodeCount;
                node2 = nodeCount + 1;
                segmentsMatrixFinal = [segmentsMatrixFinal; node1 node2];
                nodeCount = nodeCount + 1;
            end
            nodeCount = nodeCount + 1;
        end
    end
    
    % holes
    if(exist('holes', 'var'))
        for i=1:numel(holes)
            linei = holes(i);        
            lineiXj = linei.X;
            lineiYj = linei.Y;
            nodesMatrixFinal = [nodesMatrixFinal; lineiXj' lineiYj'];% lineiZj'];

            for n = 1:(size(lineiXj,2)-1)
                node1 = nodeCount;
                node2 = nodeCount + 1;
                segmentsMatrixFinal = [segmentsMatrixFinal; node1 node2];
                nodeCount = nodeCount + 1;
            end
            nodeCount = nodeCount + 1;
        end
    end
    
%% Prepend column with required indices
    [uniqueNodes, m, n] = unique(nodesMatrixFinal(segmentsMatrixFinal(:),:), 'rows');
    index = (1:size(uniqueNodes,1))';
    nodesMatrixFinal = [index uniqueNodes];    
    
    uniqueSegments = reshape(n, size(segmentsMatrixFinal));
    index = (1:size(uniqueSegments,1))';
    segmentsMatrixFinal = [index uniqueSegments];

%% Determine number nodes and segments to be transfered.
Nodes=size(nodesMatrixFinal,1);
Segments=size(segmentsMatrixFinal,1);
output12_head=[num2str(Nodes),' 2 0 0']; %2D triangulation.No attributes and boundary markers.
output22_head=[num2str(Segments),' 1']; % boundary markers are considered

%% determine points in holes
if(exist('holes', 'var'))
    holeCount = numel(holes);
    output3 = num2str(holeCount); % number of holes
    holesMatrix = zeros(holeCount,3);
    for i = 1:holeCount
        holeI = holes(i);
        pointInI = getPointInPolygon(holeI.X, holeI.Y);
        holesMatrix(i,:) = [i pointInI(1) pointInI(2)];
    end    
else
    output3='0'; %zero means no Holes
    holesMatrix = [];
end

%% INITIALIZE poly-file
% open for writing
fid = fopen(filename, 'w');

% write nodes
fprintf(fid,'%s\n',output12_head); % number of nodes
fprintf(fid,'%d %20.7f%20.7f\n',nodesMatrixFinal');

% write segments
fprintf(fid,'%s\n',output22_head); % number of segments
fprintf(fid,'%d %d %d\n',segmentsMatrixFinal'); 

% write holes
fprintf(fid,'%s\n',output3); % number of holes
fprintf(fid,'%d %20.7f%20.7f\n',holesMatrix');

%close the handle of the file
fclose(fid);

%%
end %exportPoly