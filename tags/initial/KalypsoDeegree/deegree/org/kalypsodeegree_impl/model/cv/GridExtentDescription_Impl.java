/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact:

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

                 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.model.cv;

import org.deegree.model.coverage.ElevationExtent;
import org.deegree.model.coverage.Grid;
import org.deegree.model.coverage.GridAxisDescription;
import org.deegree.model.coverage.GridExtentDescription;
import org.deegree.model.coverage.GridSpacing;
import org.deegree.model.coverage.RectifiedGrid;
import org.deegree.model.coverage.SpatialExtent;
import org.deegree.model.coverage.TemporalExtent;

/**
 * The <tt>GridExtentDescription_Impl</tt> that extends the shared 
 * <tt>DomainSetExtentDescription_Impl</tt>. It adds three/four fields to the 
 * <tt>DomainSetExtentDescription_Impl</tt>
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public class GridExtentDescription_Impl extends DomainSetExtentDescription_Impl 
                                            implements GridExtentDescription {
                                                
    private Grid grid                                   = null;
    private GridAxisDescription gridAxisDescription     = null;
    private GridSpacing gridSpacing                     = null;
    private RectifiedGrid rectifiedGrid                 = null;
    
    public GridExtentDescription_Impl(SpatialExtent spatialExtent, int dimension,
                               TemporalExtent temporalExtent, 
                               ElevationExtent elevationExtent, Grid grid,
                               GridAxisDescription gridAxisDescription,
                               GridSpacing gridSpacing)
    {
        super( spatialExtent, dimension, temporalExtent, elevationExtent );
        this.grid = grid;
        this.gridAxisDescription = gridAxisDescription;
        this.gridSpacing = gridSpacing;
    }
    
    public GridExtentDescription_Impl(SpatialExtent spatialExtent, int dimension,
                               TemporalExtent temporalExtent, 
                               ElevationExtent elevationExtent, 
                               RectifiedGrid rectifiedGrid,
                               GridAxisDescription gridAxisDescription,
                               GridSpacing gridSpacing)
    {
        super( spatialExtent, dimension, temporalExtent, elevationExtent );
        this.rectifiedGrid = rectifiedGrid;
        this.gridAxisDescription = gridAxisDescription;
        this.gridSpacing = gridSpacing;
    }
                               

    /** Either Grid or RectifiedGrid is required. Both elements list the upper
    * and lower bounds of the grid coordinates along each of the grid axes.
    * (These are integer pixel or post coordinates, expressed in the grid’s
    * internal coordinate reference system. The lower bounds are commonly
    * defined as zero.)
    *
    */
    public Grid getGrid() {
        return grid;
    }                                                

    /** identifies each axis of the grid
    *
    */
    public GridAxisDescription getGridAxisDescription() {
        return gridAxisDescription;
    }

    /** GridSpacing is required for rectified grids, but undefined for other grids.
    * It provides the ground resolution (pixel size or post spacing) along each
    * dimension of the grid, expressed in the units of the (rectified) grid’s
    * Coordinate Reference System
    *
    */
    public GridSpacing getGridSpacing() {
        return gridSpacing;
    }

    /** Either Grid or RectifiedGrid is required. Both elements list the upper
    * and lower bounds of the grid coordinates along each of the grid axes.
    * (These are integer pixel or post coordinates, expressed in the grid’s
    * internal coordinate reference system. The lower bounds are commonly
    * defined as zero.)
    *
    */
    public RectifiedGrid getRectifiedGrid() {
        return rectifiedGrid;
    }
                                                
}
