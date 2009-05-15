classdef BilateralFilter < kalypso.SmoothFilter
    methods
        function this = BilateralFilter(varargin)
            this = this@kalypso.SmoothFilter(varargin{:});
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
