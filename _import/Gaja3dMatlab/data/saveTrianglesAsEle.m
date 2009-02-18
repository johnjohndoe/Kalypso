function saveTrianglesAsEle( elements, filename )
    if(~exist('elements', 'var'))
        error('Must specify triangles.');
    end

    if(~exist('filename', 'var'))
        error('Must specify a filename.');
    end
    
    elementColumnCount = size(elements, 2);
    if(elementColumnCount ~= 3)
        error('Triangles must be a nx3 index array into coordinate arrays.');
    end
        
    [fid, message] = fopen(filename, 'w');
    if(fid == -1)
        error('Error opening file %s for writing: %s', filename, message);
    end
    
    % write elements spec
    triCount = size(elements, 1);
    fprintf(fid, '%d 3 0\n', triCount); % number of triangles, no attributes supported
    fprintf(fid, '%d %d %d %d\n', [(1:triCount)' elements]');

    fclose(fid);
end
