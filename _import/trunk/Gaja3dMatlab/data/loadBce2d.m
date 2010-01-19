function [ points, arcs, elements ] = loadBce2d( filename, varargin )
    %% read hmo
    [fid, message] = fopen(filename, 'r');
    if(fid == -1)
        error('Could not open file %s: %s', filename, message);
    end
    data = textscan(fid, '%s %f %f %f %f %f %f %f %f %f %f');
    fclose(fid);
    
    % find point rows
    entries = reshape([data{:,2:11}], size(data{1},1), 10);
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
        arcs = entries(arckind,2:6);
    end
    
    if(nargout > 2)
        elementkind = strcmp(data{:,1}, 'FE');
        elements = zeros(sum(elementkind), 12);
        elements(:,1:8) = entries(elementkind,2:9);
        frkind = strcmp(data{:,1}, 'FR');
        if(~all(frkind==0))
            elements(:,9:12) = entries(frkind,2:5);
        end
    end
end