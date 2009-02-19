function [ dataCells, fieldNames ] = loadDbf( dbfFilename )
%%LOADDBF Loads all columns from a DBase file into a struct or cell array
%   Returns the complete data in one cell array and the names of the
%   corresponding fields.
%
%   To convert from cell to struct array, use the following command:
%   structArray = cell2struct(dataCells, fieldNames, 2);
%

%% check input args
if(~exist('dbfFilename', 'var') || ~ischar(dbfFilename))
   error('Must specify a valid file name.'); 
elseif(~exist(dbfFilename, 'file'))
   error('File %s does not exist.', dbfFilename); 
end

%% open file
[dbfFileId, message] = fopen(dbfFilename, 'r', 'ieee-le');
if(dbfFileId == -1)
    error('Error opening file %s for reading: %s', dbfFilename, message);
end

%% file statistics
fseek(dbfFileId, 4, 'bof');
recordCount = fread(dbfFileId, 1, 'uint32');
headerLength = fread(dbfFileId, 1, 'uint16');
recordLength = fread(dbfFileId, 1, 'uint16');

%% read field names
fseek(dbfFileId, 32, 'bof');
fieldCount = (headerLength - 33) / 32;
fieldNames = fread(dbfFileId, [11 fieldCount], '11*uint8=>char', 21);
% replace nulls with blank strings
fieldNames(fieldNames == 0) = ' ';
% flip field name array so it is aligned with dataCells
fieldNames = cellstr(fieldNames')';

%% read field character lengths
fseek(dbfFileId, 48, 'bof');
lengths = fread(dbfFileId, [1 fieldCount], 'uint8', 31);

%% read all fields consecutively
dataCells = cell(recordCount, fieldCount);
for f=1:fieldCount    
    % jump to first field entry
    fieldOffset = headerLength + sum(lengths(1:f-1)) + 1;
    fseek(dbfFileId, fieldOffset, 'bof');    
    % read number of characters according to length of field
    lengthF = lengths(f);
    formatString = sprintf('%d*uint8=>char', lengthF);
    % read all records for this field, always skipping to next
    skip = recordLength - lengthF;
    data = fread(dbfFileId, [lengthF recordCount], formatString, skip);
    % justify left to remove leading and trailing spaces
    dataCells(:,f) = cellstr(strjust(data', 'left'));
end

%% close file
fclose(dbfFileId);

end %LOADDBF