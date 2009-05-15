classdef SimpleFeatureDetector < kalypso.FeatureDetector
    properties
        morph_ops
        bp_remove
        bp_hole_remove
    end
    
    methods
       function this = SimpleFeatureDetector(varargin)
           this = this@kalypso.FeatureDetector(varargin{:});
       end
       
       function configure(this, varargin)
            this.configure@kalypso.FeatureDetector(varargin{:});
            
            p = inputParser;
            p.KeepUnmatched = true;
            p.addParamValue('bp_remove', 400, @isnumeric);
            p.addParamValue('bp_hole_remove', 100, @isnumeric);
            p.addParamValue('morph_ops', 'thicken,~thicken,thicken,~thicken,hbreak,~diag,spur(Inf),~spur(Inf)', @isstr);
            p.parse(varargin{:});
            
            this.bp_remove = p.Results.bp_remove;
            this.bp_hole_remove = p.Results.bp_hole_remove;
            
            morph_ops_s = p.Results.morph_ops;
            morph_ops_c = regexp(morph_ops_s, ',', 'split');
            count = numel(morph_ops_c);
            this.morph_ops = cell(count, 1);
            for i=1:count
                morph_op_s = morph_ops_c{i};
                morph_op_param = regexp(morph_op_s, '[()]', 'split');
                if(numel(morph_op_param)>2)
                    morph_op_param{2} = str2double(morph_op_param{2});
                    morph_op_param(3:end) = [];
                end
                if(strncmp(morph_op_param{1}, '~', 1))
                    morph_op_param{1} = morph_op_param{1}(2:end);
                    this.morph_ops{i} = @(breakpoints) ~bwmorph(~breakpoints, morph_op_param{:});
                else
                    this.morph_ops{i} = @(breakpoints) bwmorph(breakpoints, morph_op_param{:});
                end
            end
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
           
           % process with morphological filters
           for i=1:numel(this.morph_ops)
               breakpoints = this.morph_ops{i}(breakpoints);
           end
           
           % remove 4-connected areas and holes
           if(this.bp_remove > 0)
              minsize = this.bp_remove / grid.dx;
              breakpoints = bwareaopen(breakpoints, minsize, 4);
           end
           if(this.bp_hole_remove > 0)
              minsize = this.bp_hole_remove / grid.dx;
              breakpoints = ~bwareaopen(~breakpoints, minsize, 4);
           end
       end
    end
end 
