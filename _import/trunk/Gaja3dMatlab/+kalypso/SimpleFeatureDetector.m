classdef SimpleFeatureDetector < kalypso.FeatureDetector
   methods
       function this = SimpleFeatureDetector(varargin)
           this = this@kalypso.FeatureDetector(varargin{:});
       end
       
       function breakpoints = process(this, grid)
           [ax, ay, mag, highThresh, lowThresh] = this.getInputs(grid);
           if(isempty(mag))
               breakpoints = zeros(size(mag));
               return;
           end
           % filter pixels above low threshold
           aboveLow = mag > lowThresh;
           
           % keep pixels with 8-connectivity to pixels above high
           % threshold
           [aboveHighR, aboveHighC] = find(mag > highThresh);
           breakpoints = bwselect(aboveLow, aboveHighC, aboveHighR, 8);
           
           % remove isolated pixels in exterior and interior
           breakpoints = bwmorph(breakpoints, 'fill');
           breakpoints = bwmorph(breakpoints, 'clean');
 
           % remove spur pixels and
           % remove 4-connected areas with less than (minsize) pixels
           minsize = 400 / grid.dx;
           breakpoints = bwmorph(breakpoints, 'spur');
           breakpoints = ~bwmorph(~breakpoints, 'spur');
           breakpoints = bwmorph(breakpoints, 'diag');
           breakpoints = ~bwmorph(~breakpoints, 'diag');
           breakpoints = bwareaopen(breakpoints, minsize, 4);
           breakpoints = ~bwareaopen(~breakpoints, minsize, 4);

           % remove interior pixels, keep boundary pixels of areas
           %breakpoints = bwmorph(breakpoints, 'remove', Inf);
       end
   end
end 
