function [ points ] = loadPointData( filespec, hasheader, params )
%LOADPOINTDATA  Loads point data from shape (.shp) or text (.txt, .xyz) file
%               distinguish by type and ascii delimiter
%   file      - valid filename
%
%
%   The following parameters are only used when loading a text file
%
%   hasheader - if 1, also read first line of file (defaults to 1 = has header)
%   params    - cell array of strings (param/value pairs) as described for textscan
    
    if(~exist('filespec','var'))
        error('Must specify files.');
    end
    
    if(~isstruct(filespec))
        pathstr = fileparts(filespec);
        files = dir(filespec);
    else
        files = filespec;
    end
    
    if(isempty(files))
        error('No files found for %s', filespec);
    end
    
    if(~exist('hasheader','var'))
        hasheader = 1;
    end   
    
    ZIPEXT = '.zip';
    SHPEXT = '.shp';
    TXTEXT = '.txt';
    XYZEXT = '.xyz';
    HMOEXT = '.hmo';
    
    points = cell(numel(files), 1);
    for i=1:numel(files)
        if(exist('pathstr','var'))
            file = fullfile(pathstr, files(i).name);
        else
            file = files(i).name;
        end
        
        [dummy, name, ext] = fileparts(file);
    
        switch(lower(ext))
            case ZIPEXT
                tmp_dir = tempname;
                tempFiles = unzip(file, tmp_dir);
                points{i} = loadPointData(cell2struct(tempFiles, 'name', numel(tempFiles)));
                rmdir(tmp_dir, 's');
            case SHPEXT
                %read shapefile
                points{i} = loadPointShape(file);
            case {TXTEXT,XYZEXT}
                fid = fopen(file);
                if (fid == -1)
                    error('File %s not found or permission denied.', file);
                end
                line = fgetl(fid);
                if (~ischar(line))
                    warning('GAJA3D:General', 'File %s contains no header and no data.', file);
                end;

                if(~hasheader)
                    % go back to beginning of file
                    fseek(fid, 0, 'bof');
                end

                % find delimiting tokens
                [matchstr, start_idx, end_idx, extents, tokens] = regexp(line, '[,\t\s]*', 'split');

                if(isempty(tokens))
                    error('Could not find any delimiters (comma, tab, spaces) in file %s', file);
                end

                if(start_idx(1) == 1)
                    % first token is at beginning of line, remove
                    tokens = tokens(2:end);
                end

                % construct format string
                format = '%f';
                for j=1:size(tokens, 2)
                    format = [format tokens{j} '%f'];
                end

                if(exist('params','var'))
                    % construct command with params
                    if(~iscell(params))
                        error('Last argument must be a cell array of strings (param/value pairs).');
                    end
                    paramString = '';
                    for j=1:size(params, 2)
                        paramString = [paramString ', ''' params{j} ''''];
                    end

                    %execute textscan with additional params
                    eval(sprintf('data = textscan(fid, format, ''CollectOutput'', 1%s);', paramString));
                else
                    %execute textscan
                    data = textscan(fid, format, 'CollectOutput', 1);
                end

                % data is 1x1 cell array
                points{i} = data{1};

                %close file
                fclose(fid);
            case {HMOEXT}
                points{i} = loadHmo(file);
            otherwise
                fprintf(1, 'File extension %s not recognized.\n', ext);
        end    
    end
    
    points = vertcat(points{:});
%%
end %LOADPOINTDATA