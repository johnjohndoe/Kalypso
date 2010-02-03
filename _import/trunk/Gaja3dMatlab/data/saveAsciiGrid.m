function saveAsciiGrid( filename, grid, fieldname )
    if(~exist('fieldname','var'))
        fieldname = 'Z';
    end

    fid = fopen(filename, 'w');
    
    nrows = size(grid.Z, 1);
    ncols = size(grid.Z, 2);
    fprintf(fid, 'ncols %d\n', ncols);
    fprintf(fid, 'nrows %d\n', nrows);
        
    point11 = [1 1 1] * grid.refmat;
    fprintf(fid, 'xllcorner %-20.7f\n', point11(1));
    fprintf(fid, 'yllcorner %-20.7f\n', point11(2));
    
    fprintf(fid, 'cellsize %d\n', grid.dx);

    nodata = -9999;
    fprintf(fid, 'nodata_value %0.3f\n', nodata);
    
    Z = grid.(fieldname);
    Z(isnan(Z)) = nodata;
    
    %% flip along first axis, i.e. print rows in reverse order
    for r=nrows:-1:1
        fprintf(fid, '%0.3f ', Z(r,:));
        fprintf(fid, '\n');
    end
    
    fclose(fid);
end