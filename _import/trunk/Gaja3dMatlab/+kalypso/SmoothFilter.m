classdef SmoothFilter < handle
    properties
        hsize
    end
    
    methods
        function this = SmoothFilter(varargin)
            this.configure(varargin{:});
        end
        
        function configure(this, varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('smooth', 9, @isnumeric);
            p.parse(varargin{:});
            this.hsize = p.Results.smooth;
        end
    end
end

