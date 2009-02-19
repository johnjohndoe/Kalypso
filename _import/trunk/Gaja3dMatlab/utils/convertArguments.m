function [ varargin ] = convertArguments( varargin )
    % loop over key-value input arguments
    % and try to convert all values to numbers
    for i=1:2:nargin
        key = varargin{i};
        value = varargin{i+1};
        if(strcmp(key, 'tiles'))
            % process specified tiles
            s2 = regexp(value, '\,', 'split');
            varargin{i+1} = str2double(s2);
        else
            try
                valueNew = str2double(value);
                if(~isnan(valueNew))
                    varargin{i+1} = valueNew;
                end
            catch %#ok<CTCH>
                % ignore
            end
        end
    end
end
