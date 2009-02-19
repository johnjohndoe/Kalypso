classdef GaussianFilter
    properties
        hsize
        name = 'gauss';
    end % private properties

    methods
        function this = GaussianFilter(varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('smooth', 9, @isnumeric);
            p.parse(varargin{:});
            this.hsize = p.Results.smooth;
        end
        
        function Z = process(this, grid)
            sigma = -this.hsize*this.hsize/(2*log(0.0001)); % determine sigma
            Z = imfilter(grid.Z, fspecial('gaussian', floor(this.hsize), sigma), 'replicate', 'conv');
        end
    end
end 
