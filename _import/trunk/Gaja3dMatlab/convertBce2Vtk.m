function convertBce2Vtk(indir, outdir, fromindex, toindex)
    for i=fromindex:toindex
        filename = sprintf('%s/A%04d.2d', indir, i);
        [points, arcs, elements] = loadBce2d(filename, 'VA');
        [vertices, faces] = plotBce2d(points, arcs, elements);
        saveMeshAsVtk(sprintf('%s/A%04d', outdir, i), vertices, faces);
    end
end