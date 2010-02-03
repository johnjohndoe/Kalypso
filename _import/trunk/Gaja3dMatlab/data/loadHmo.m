function [ points ] = loadHmo( hmofile )
    %% read hmo, currently only reads P: entries
    [fid, message] = fopen(hmofile, 'r');
    if(fid == -1)
        error('Could not open hmo-file %s: %s', hmofile, message);
    end
    eleMatrix = textscan(fid, '%s %f %f %f %f');
    fclose(fid);
    
    % find point rows
    entries = reshape([eleMatrix{:,2:5}], size(eleMatrix{1},1), 4);
    pkind = strcmp(eleMatrix{:,1}, 'P:');
    points = entries(pkind,2:4);
end