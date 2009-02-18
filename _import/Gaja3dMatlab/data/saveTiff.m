function saveTiff( filename, grid )
% warning: grid Z values will be saved in single precision!
    import javax.media.jai.*;
    import java.awt.image.renderable.*;
    import com.sun.media.jai.codec.*;
    
    [pathstr, name, ext] = fileparts(filename);
    if(strcmpi(ext, '.tif'))
        filename = [pathstr name]; % strip file extension
    end
    
    buf = single(grid.Z);
    dbuffer = DataBufferFloat(buf(:), numel(buf));
    dim = size(buf);
    % TYPE_DOUBLE does not seem to be supported at the time
    sampleModel = RasterFactory.createBandedSampleModel(java.awt.image.DataBuffer.TYPE_FLOAT, dim(1), dim(2), 1);
    colorModel = PlanarImage.createColorModel(sampleModel);
    raster = RasterFactory.createWritableRaster(sampleModel, dbuffer, java.awt.Point(0,0));
    tiledImage = TiledImage(0, 0, dim(1), dim(2), 0, 0, sampleModel, colorModel);
    tiledImage.setData(raster);
    JAI.create('filestore', tiledImage, [filename '.tif'], 'TIFF');
    
    fid = fopen([filename '.tfw'], 'w');
    Cinv = [0  1  1; 1  0  1; 0  0  1];
    W = grid.refmat' * Cinv;
    fprintf(fid, '%20.8f\n', W);
    fclose(fid);
    
end
