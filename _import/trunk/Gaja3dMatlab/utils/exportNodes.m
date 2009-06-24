function exportNodes(nodes, filename)
%% EXPORTNODES export a node file for triangle
%   first and second column of nodes is expected to be the x and y
%   coordinates, an index column will be prepended, all extra columns
%   will be written as attributes
%   filename will get a .node extension
%

    colCount = size(nodes, 2);
    if(colCount < 2)
        error('nodes must have at least 2 columns');
    end
    
    % open for writing
    
    nodeFilename = [filename '.node'];
    fid = fopen(nodeFilename, 'w');
    if(fid == -1)
        error('Could not open file %s', nodeFilename);
    end
    
    %prepend index column
    index = (1:size(nodes, 1))';
    nodes = [index nodes];
    
    % number of attributes to be written
    attCount = colCount - 2; % do not count x and y
    attFormat = ' %20.2f'; % format 
    formatString = ['%d' repmat(attFormat, 1, colCount) '\n'];
    
    % write header
    % [number of nodes] [number of coordinates (2)] [number of attributes]
    % [number of boundary markers (0)]
    fprintf(fid, '%d 2 %d 0\n', size(nodes, 1), attCount);
    
    % write node coordinates and attributes
    fprintf(fid, formatString, nodes');

    %close the handle of the file
    fclose(fid);
end