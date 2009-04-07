classdef BilateralFilter
    properties
        hsize
        name = 'bilateral';
    end % private properties

    methods
        function this = BilateralFilter(varargin)
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('smooth', 9, @isnumeric);
            p.parse(varargin{:});
            this.hsize = p.Results.smooth;
        end
        
        function Z = process(this, grid)
            minZ = min(grid.Z(:));
            norm0 = grid.Z - minZ;
            maxNorm0 = max(norm0(:));
            norm01 = norm0 / maxNorm0;
            sigma = -this.hsize*this.hsize/(2*log(0.0001)); % determine sigma
            Z = bfilter2(norm01, this.hsize/2, [sigma, 0.1]) * maxNorm0 + minZ;
        end
    end
end 
