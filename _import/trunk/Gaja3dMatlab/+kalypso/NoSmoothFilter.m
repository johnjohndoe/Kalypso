classdef NoSmoothFilter < kalypso.SmoothFilter
    properties
        name = 'none';
    end % private properties

    methods
        function this = NoSmoothFilter(varargin)
            this = this@kalypso.SmoothFilter(varargin{:});
        end
        
        function Z = process(this, grid) %#ok<MANU>
            % do nothing
            Z = grid.Z;
        end
    end
end 
