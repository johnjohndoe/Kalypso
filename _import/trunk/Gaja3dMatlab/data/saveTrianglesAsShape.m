function saveTrianglesAsShape( filename, elements, Xtri, Ytri)
%SAVETRIANGLESASSHAPE Summary of this function goes here
%   Detailed explanation goes here

    %initialize cell arrays
    totalTriCount = size(elements, 1);
    X = cell(totalTriCount, 1);
    Y = cell(totalTriCount, 1);
    B = cell(totalTriCount, 1);
    ID = cell(totalTriCount, 1);
    
    for t=1:totalTriCount
        % make last coordinates equal to first
        X{t} = Xtri(elements(t,[1 2 3 1])); % x coordinates of triangle t
        %X{t} = [ X{t}; X{t}(1) ];
        Y{t} = Ytri(elements(t,[1 2 3 1])); % y coordinates of triangle t
        %Y{t} = [ Y{t}; Y{t}(1) ];
        % convert to clockwise ordering
        [X(t), Y(t)] = poly2cw(X(t), Y(t));
        % calculate bounding box
        B{t} = [min(X{t}), min(Y{t}); max(X{t}), max(Y{t})];
        % set consecutive id
        ID{t} = t;
    end
    
    % create geostruct with polygon geometry from cell arrays
    tripoly = struct('Geometry', 'Polygon', 'BoundingBox', B, 'X', X, 'Y', Y, 'ID', ID);
    
    % write shape file with id in dBase table
    shapewrite(tripoly, filename, 'DbfSpec', makedbfspec(tripoly));
    
end %SAVETRIANGLESASSHAPE