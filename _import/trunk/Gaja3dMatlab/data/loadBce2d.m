function [ points, arcs, elements ] = loadBce2d( filename, varargin )
    %% read hmo
    [fid, message] = fopen(filename, 'r');
    if(fid == -1)
        error('Could not open file %s: %s', filename, message);
    end
    data = textscan(fid, '%s %f %f %f %f %f %f %f %f %f %f');
    fclose(fid);
    
    % find point rows
    lastIndex = 3;
    while(size(data{lastIndex+1},1) == size(data{lastIndex},1))
        lastIndex = lastIndex + 1;
        if(lastIndex == size(data,2))
            break;
        end
    end
    entries = reshape([data{:,2:lastIndex}], size(data{1},1), lastIndex-1);
    pkind = strcmp(data{:,1}, 'FP');
    points = zeros(sum(pkind), 4+4*(nargin-1));
    points(:,1:4) = entries(pkind,1:4);
    points = sortrows(points, 1);
    points(:,1) = [];
    
    for i=1:(nargin-1)
        xkind = strcmp(data{:,1}, varargin{i});
        points(:,1+i*3:1+(1+i)*3) = entries(xkind,2:5);
    end
    
    if(nargout > 1)
        arckind = strcmp(data{:,1}, 'AR');
        arcs = entries(arckind,2:5);
    end
    
    if(nargout > 2)
        elementkind = strcmp(data{:,1}, 'FE');
        elements = zeros(sum(elementkind), 12);
        elements(:,1:8) = entries(elementkind,2:9);
        isActive = elements(:,2) > 0;
        inactiveIds = find(~isActive);
        for i=1:numel(inactiveIds)
            setToZeroLeft = arcs(:,3) == inactiveIds(i);
            setToZeroRight = arcs(:,4) == inactiveIds(i);
            arcs(setToZeroLeft,3) = 0;
            arcs(setToZeroRight,4) = 0;
        end
        frkind = strcmp(data{:,1}, 'FR');
        if(~all(frkind==0))
            elements(:,9:12) = entries(frkind,2:5);
        end
    end
end