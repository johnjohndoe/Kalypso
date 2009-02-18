function points = loadPointShape( filename )
%LOADPOINTSHAPE Load point data from a Point or PointZ shape file

    [pathstr,name,ext] = fileparts(filename);

    shapefileExtensions = {'.shp','.shx','.dbf'};
    if isempty(ext)
        basename = filename;
    elseif any(strcmpi(ext,shapefileExtensions))
        basename = fullfile(pathstr,name);
    else
        basename = filename;
    end

    shpFileId = fopen([basename '.shp']);
    if (shpFileId == -1)
        shpFileId = fopen([basename '.SHP']);
    end;

    try
        fileHeaderLength = 100; %for shape files in general

        fseek(shpFileId,24,'bof'); %file length
        fileLength = 2 * fread(shpFileId,1,'uint32','ieee-be');

        fseek(shpFileId,32,'bof'); %shape type code
        headerTypeCode = fread(shpFileId,1,'uint32','ieee-le');

        switch(headerTypeCode)
            case 1 % Point
                loadPointMethod = @readPoints;
                recordLength = 28;
            case 11 % PointZ
                loadPointMethod = @readPointsZ;
                recordLength = 36;
            otherwise
                error('Shape geometry type neither Point nor PointZ.');
        end

        recordCount = (fileLength - fileHeaderLength) / recordLength; %calculate from file length
        recordsStart = fileHeaderLength; %beginning of point records

        fseek(shpFileId, recordsStart + 7,'bof'); % start reading first x value
        recordLength = fread(shpFileId, 1);
        fread(shpFileId, 4);
        if(recordLength == 18)
            % maybe we have M values
            loadPointMethod = @readPointsZM;
        end
        points = loadPointMethod();
    catch e
        fclose(shpFileId);
        rethrow(e);
    end
    fclose(shpFileId);
    
    function points = readPointsZ()
        %% read all PointZ from file
        formatString = '3*double'; %read x, y and z (3*8 bytes) for each record, ignore m
        points = (fread(shpFileId,[3 recordCount],formatString,12,'ieee-le'))';
    end

    function points = readPointsZM()
        %% read all PointZ from file
        formatString = '4*double'; %read x, y, z and m (4*8 bytes) for each record
        points = (fread(shpFileId,[4 recordCount],formatString,12,'ieee-le'))';
    end

    function points = readPoints()
        %% read all Point from file
        points = zeros(recordCount, 3) * NaN; %init points
        formatString = '2*double'; %read x and y (2*8 bytes) for each record
        points(:,1:2) = (fread(shpFileId,[2 recordCount],formatString,12,'ieee-le'))'; %skip 12 bytes        
        dbfFileId = fopen([basename '.dbf'],'r','ieee-le');
        if (dbfFileId == -1)
            dbfFileId = fopen([basename '.DBF'],'r','ieee-le');
        end

        if(dbfFileId ~= -1)
            % file info
            fseek(dbfFileId,0,'bof');
            version = fread(dbfFileId,1,'uint8');
            year  = fread(dbfFileId,1,'uint8') + 1900;
            month = fread(dbfFileId,1,'uint8');
            day   = fread(dbfFileId,1,'uint8');
            dateVector = datevec(sprintf('%d/%d/%d',month,day,year));
            numRecords   = fread(dbfFileId,1,'uint32');
            headerLength = fread(dbfFileId,1,'uint16');
            recordLength = fread(dbfFileId,1,'uint16');

            lengthOfLeadingBlock    = 32;
            lengthOfDescriptorBlock = 32;
            lengthOfTerminator      =  1;
            fieldNameOffset         = 16;  % Within table field descriptor
            fieldNameLength         = 11;

            % Get number of fields.
            fseek(dbfFileId,8,'bof');
            headerLength = fread(dbfFileId,1,'uint16');
            numFields = (headerLength - lengthOfLeadingBlock - lengthOfTerminator)...
                           / lengthOfDescriptorBlock;

            % Read field lengths.
            fseek(dbfFileId,lengthOfLeadingBlock + fieldNameOffset,'bof');
            lengths = fread(dbfFileId,[1 numFields],'uint8',lengthOfDescriptorBlock - 1);

            % Read the field names.
            fseek(dbfFileId,lengthOfLeadingBlock,'bof');
            allFieldNames = fread(dbfFileId,[fieldNameLength numFields],...
                         sprintf('%d*uint8=>char',fieldNameLength),...
                         lengthOfDescriptorBlock - fieldNameLength);
            allFieldNames(allFieldNames == 0) = ' '; % Replace nulls with blanks
            allFieldNames = cellstr(allFieldNames');
            indexZ = strmatch('Z',allFieldNames,'exact');
            lengthZ = lengths(indexZ);

            fieldOffset = headerLength ...
                          + sum(lengths(1:(indexZ-1))) ... % sum of all field lengths before Z
                          + 1; % deletion
            fseek(dbfFileId,fieldOffset,'bof');
            formatString = sprintf('%d*uint8=>char',lengthZ);
            skip = recordLength - lengthZ;

            data = fread(dbfFileId,[lengthZ numRecords],formatString,skip);

            for n=1:size(data,2)
                points(n,3)=str2double(data(:,n)');
            end
        end
    end

end %LOADPOINTSHAPE

