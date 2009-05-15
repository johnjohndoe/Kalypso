classdef NoFeatureDetector < kalypso.FeatureDetector
   methods
       function this = NoFeatureDetector(varargin)
           this = this@kalypso.FeatureDetector(varargin{:});
       end
       
       function breakpoints = process(this, grid) %#ok<MANU>
           % do nothing
           breakpoints = zeros(size(grid.Z));
       end
   end
end 
