classdef FeatureDetector
   properties (Constant)
       DEFAULT = kalypso.SimpleFeatureDetector();
   end
   
   properties
       highThresh
       lowThresh
   end
   
   methods
       function this = FeatureDetector(varargin)
           % parse arguments
           p = inputParser;
           p.KeepUnmatched = true;
           p.addParamValue('highThresh', [], @isnumeric);
           p.addParamValue('lowThresh', [], @isnumeric);
           p.parse(varargin{:});
           this.lowThresh = p.Results.lowThresh;
           this.highThresh = p.Results.highThresh;
       end
       
       function [ax, ay, mag, highThresh, lowThresh] = getInputs(this, grid)
           slope = grid.slope;
           ay = slope(:,:,1);
           ax = slope(:,:,2);
           
           % compute magnitude of slope as geometric mean of slope ax,ay
           mag = sqrt((ax.*ax) + (ay.*ay));
           maxslope = max(mag(:));
           
           if(isempty(this.highThresh))
               highThresh = 0.7 * maxslope;
           elseif(this.highThresh > maxslope)
               % higher values do not make sense and cause wrong results!
               highThresh = maxslope - 0.0001;
           else
               highThresh = this.highThresh;
           end
           
           if(isempty(this.lowThresh))
               lowThresh = 0.1 * maxslope;
           elseif(this.lowThresh > this.highThresh)
               % restrict lowThresh to be equal to highThresh, this can
               % also cause wrong results
               lowThresh = this.highThresh;
           else
               lowThresh = this.lowThresh;
           end
       end
   end
   
   methods (Abstract)
       features = process(this, grid)
   end
end 
