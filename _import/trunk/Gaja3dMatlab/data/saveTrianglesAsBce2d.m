function [xshift, yshift] = saveTrianglesAsBce2d( filename, elements, Xtri, Ytri, Ztri, xshift, yshift )
%SAVETRIANGLESASBCE2D Saves triangles as BCE2d-file
%   elements           nx3 indices into coordinate arrays
%   Xtri, Ytri, Ztri   nx1 point coordinates
%   filename           the name of the file to save to
%   xshift, yshift     optional - shifting of coordinates
%   
%   The points will automatically be shifted so that the lowest x and
%   y coordinates are 100 if no shifting is supplied and if they
%   are equal to or exceed 100000.
%

%% check parameters and error handling
    if(~exist('elements', 'var') || ~exist('Xtri', 'var') || ~exist('Ytri', 'var'))
        error('Must specify triangles and point coordinates.');
    end    
    if(~exist('Ztri', 'var'))
        error('Must specify point z-coordinates.');
    end
    
    if(~exist('filename', 'var'))
        error('Must specify a filename.');
    end
    
    elementColumnCount = size(elements, 2);
    if(elementColumnCount ~= 3)
        error('Triangles must be a nx3 index array into coordinate arrays.');
    end
    
    xCount = size(Xtri, 1);
    yCount = size(Ytri, 1);
    zCount = size(Ztri, 1);    
    if(xCount ~= yCount || yCount ~= zCount)
        error('Point coordinate array sizes must match.');
    end
    
    maxCoordinateIndex = max(max(elements));
    if(maxCoordinateIndex > xCount)
        error('Triangle point indices too big for provided points.');
    end
    
    [fid, message] = fopen(filename, 'w');
    if(fid == -1)
        error('Error opening file %s for writing: %s', filename, message);
    end
    
%%  shifting
    
    % apply shifting of gauss-krüger for bce2d
    % shift lowest coordinates to 100 if not supplied
    % and if they are equal to or exceed 100000
    if(~exist('xshift','var'))
        if(max(Xtri) >= 100000)
            xshift = 100 - min(Xtri);
        else
            xshift = 0;
        end
    end
    if(~exist('yshift','var'))
        if(max(Ytri) >= 100000)
            yshift = 100 - min(Ytri);
        else
            yshift = 0;
        end
    end
    
    % do shift
    Xtri = Xtri + xshift;
    Ytri = Ytri + yshift;

%% write points and elements (easy)
    % point ids
    nodeCount = xCount; % same as others
    nodeIds = 1:nodeCount;
    % write point coordinates
    fprintf(fid, 'FP%10.0f%20.7f%20.7f%20.7f\n', [nodeIds' Xtri Ytri Ztri]');

    % matrix is already transposed for writing        
    totalTriCount = size(elements, 1);
    eleMatrixFinal = zeros(5, totalTriCount);
    % element ids
    eleMatrixFinal(1,:) = 1:totalTriCount;
    % one as initial rougness class
    eleMatrixFinal(2,:) = 1;
    % write elements
    fprintf(fid, 'FE%10.0f%10.0f%10.0f%10.0f%15.7f\n', eleMatrixFinal);    

%%  calculate and write arcs (hard part)
    arcs = sparse(nodeCount, nodeCount); % sparse node adjacency matrix

    arcCount = 0;
    arcIndex = [1 2; 2 3; 3 1]; % index pairs for traversing triangle arcs
    for t=1:totalTriCount
        for node=1:3
            arcNodes = elements(t, arcIndex(node,:));
            node1 = arcNodes(1);
            node2 = arcNodes(2);
            if(arcs(node1, node2))
                continue; % this arc already exists
            end
            arcCount = arcCount + 1;
            arcs(node1, node2) = 1;
            arcs(node2, node1) = 1;
            x1 = Xtri(node1); % x coordinates of triangle t
            y1 = Ytri(node1); % y coordinates of triangle t
            x2 = Xtri(node2); % x coordinates of triangle t
            y2 = Ytri(node2); % y coordinates of triangle t
            vx1 = x2 - x1;
            vy1 = y2 - y1;
            [row1, col1] = find(elements == node1);
            [row2, col2] = find(elements == node2);
            adjacentElements = intersect(row1, row2);
            adjCount = numel(adjacentElements);            
            left = 0;
            right = 0;
            for e=1:adjCount
                el = adjacentElements(e);
                nodes = elements(el, :);
                node3 = setdiff(nodes, arcNodes);
                x3 = Xtri(node3); % x coordinates of triangle t
                y3 = Ytri(node3); % y coordinates of triangle t
                vx2 = x3 - x1;
                vy2 = y3 - y1;
                magnitude = vx1 * vy2 - vy1 * vx2;
                if( magnitude > 0 )
                    left = el;
                else
                    right = el;
                end
            end
            fprintf(fid, 'AR %d %d %d %d %d\n', [arcCount node1 node2 left right]);
        end        
    end
    
%% write roughness class
    %append a uniform roughness class 1 in blue color (MicroStation: blue=1)
    %additional fields are left blank
    roughness='RK    1 Gaja3D Standard Roughness Class          1         0         0    0    0';
    fprintf(fid, '%c', roughness);

%% done, close file
    status = fclose(fid);
    if(status == -1)
        warning('Could not close file %s.', filename);
    end
end %SAVETRIANGLESASBCE2D
