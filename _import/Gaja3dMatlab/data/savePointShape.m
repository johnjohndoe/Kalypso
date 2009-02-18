function savePointShape( filename, points )
    X = points(:,1);
    Y = points(:,2);
    Z = points(:,3);

    % shape file magic numbers (PointZ)
    recordCount = size(points, 1);
    recordNumbers = 1:recordCount;
    contentLengthInWords = 14;
    recordLength = 36;
    headerLength = 100;
    fileCode = 9994;
    fileLengthInWords = (recordCount * recordLength + headerLength) / 2;
    version  = 1000;
    shapeType = 11;
    boundingBox = [min(X) min(Y) max(X) max(Y)];
    bytes0thru27 = int32([fileCode 0 0 0 0 0 fileLengthInWords]);
    bytes28thru35 = int32([version shapeType]);
    bytes36thru99 = [boundingBox 0 0 0 0];
    
    % write the shape file
    fid = fopen([filename '.shp'], 'w', 'ieee-be');
    % file header
    fwrite(fid, bytes0thru27, 'int32', 'ieee-be');
    fwrite(fid, bytes28thru35, 'int32', 'ieee-le');
    fwrite(fid, bytes36thru99, 'double', 'ieee-le');
    % record headers
    fseek(fid, headerLength - 28, 'bof');
    fwrite(fid, int32([recordNumbers; ones(1, recordCount) * contentLengthInWords]), '2*int32', 28, 'ieee-be');
    fseek(fid, headerLength - 24, 'bof');
    fwrite(fid, int32(ones(1, recordCount)*shapeType), 'int32', 32, 'ieee-le');
    % records
    fseek(fid, headerLength, 'bof');
    fwrite(fid, [X Y Z]', '3*double', 12, 'ieee-le');
    fclose(fid);
    
    % shape index magic numbers
    startIdxInWords = headerLength / 2;
    endIdxInWords = fileLengthInWords - 18;
    indices = startIdxInWords:18:endIdxInWords;
    index = [indices; ones(1, recordCount) * contentLengthInWords];
    fileLengthInWords = headerLength / 2 + 4 * size(index,2);
    bytes0thru27 = int32([fileCode 0 0 0 0 0 fileLengthInWords]);

    % write shape index
    fid = fopen([filename '.shx'], 'w', 'ieee-be');
    fwrite(fid, bytes0thru27, 'int32', 'ieee-be');
    fwrite(fid, bytes28thru35, 'int32', 'ieee-le');
    fwrite(fid, bytes36thru99, 'double', 'ieee-le');
    fwrite(fid, int32(index), 'int32', 'ieee-be');
    fclose(fid);
    
    % dBASE file magic numbers
    version = 3;
    dateVector = datevec(date); % current date
    year  = dateVector(1);
    month = dateVector(2);
    day   = dateVector(3);
    fieldLength = length(sprintf('%d', recordCount)); % biggest id
    recordLength = 1 + fieldLength;
    headerLength = 32 + 32 * 1 + 1; % one field
    formatString = sprintf('%s%dd', '%', recordLength);
    
    %write dBASE file with index field (ID)
    fid = fopen([filename '.dbf'], 'w', 'ieee-le');
    fwrite(fid, [version year-1900 month day], 'uint8');
    fwrite(fid, recordCount, 'uint32');
    fwrite(fid, headerLength, 'uint16');
    fwrite(fid, recordLength, 'uint16');
    fwrite(fid, zeros(1,20), 'uint8'); % reserved
    fwrite(fid, ['ID' zeros(1,9)], 'uchar'); % pad field name to 11 chars
    fwrite(fid, 'N', 'uchar'); % type: number
    fwrite(fid, zeros(1,4), 'uint8'); % reserved
    fwrite(fid, fieldLength, 'uint8');
    fwrite(fid, 0, 'uint8');
    fwrite(fid, zeros(1,14), 'uint8'); % reserved
    fwrite(fid, hex2dec('0D') ,'uint8'); % end of header
    fprintf(fid, formatString, recordNumbers); % records
    fwrite(fid, hex2dec('1A'), 'uint8'); % end of file
    fclose(fid);
end
