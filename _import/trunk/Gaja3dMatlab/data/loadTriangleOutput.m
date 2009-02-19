function [ eleMatrix, Xtri, Ytri, varargout ] = loadTriangleOutput( dataset )
    %% read elements
    elefile = [dataset '.ele'];
    [fid, message] = fopen(elefile, 'r');
    if(fid == -1)
        error('Could not open element file %s: %s', elefile, message);
    end
    % read header and discard
    textscan(fid, '%f', 3);
    eleMatrix = textscan(fid, '%d %d %d %d');
    fclose(fid);
    eleMatrix = reshape([eleMatrix{:,2:4}], size(eleMatrix{1},1), 3);
    
    if(nargout > 1)
    %% read nodes
        nodefile = [dataset '.node'];
        
        [fid, message] = fopen(nodefile, 'r');
        if(fid == -1)
            error('Could not open node file %s: %s', nodefile, message);
        end

        % read header
        header = textscan(fid, '%f', 4);
        attCount = header{1}(3);
        boundaryMarker = header{1}(4);
        
        % id, x, y (3) + attributes + boundary markers
        columnCount = 3+attCount+boundaryMarker;
        
        % first column is integer, remaining columns are doubles
        formatString = ['%d ' repmat('%f ',1, columnCount - 1)];
        nodeMatrix = textscan(fid, formatString);
        fclose(fid);

        % extract columns
        Xtri = nodeMatrix{:,2};
        Ytri = nodeMatrix{:,3};
        for i=1:(nargout-3)
            if(attCount >= i)
                varargout{i} = nodeMatrix{:, i+3};
            else
                varargout{i} = zeros(size(Xtri));
            end
        end
    end
end