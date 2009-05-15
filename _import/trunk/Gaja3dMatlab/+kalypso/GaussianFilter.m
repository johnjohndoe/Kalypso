classdef GaussianFilter < kalypso.SmoothFilter

    methods
        function this = GaussianFilter(varargin)
            this = this@kalypso.SmoothFilter(varargin{:});
        end
        
        function Z = process(this, grid)
            sigma = -this.hsize*this.hsize/(2*log(0.0001)); % determine sigma
            Z = imfilter(grid.Z, fspecial('gaussian', floor(this.hsize), sigma), 'replicate', 'conv');
        end
    end
end 
