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

import org.deegree.model.coverage.DomainSetExtentDescription;
import org.deegree.model.coverage.ElevationExtent;
import org.deegree.model.coverage.SpatialExtent;
import org.deegree.model.coverage.TemporalExtent;

/**
 * Describes the extents that are common to all coverage types.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class DomainSetExtentDescription_Impl implements DomainSetExtentDescription
{

  private int dimension = 0;

  private ElevationExtent elevationExtent = null;

  private SpatialExtent spatialExtent = null;

  private TemporalExtent temporalExtent = null;

  private boolean temporal = false;

  public DomainSetExtentDescription_Impl( SpatialExtent spatialExtent, int dimension,
      TemporalExtent temporalExtent, ElevationExtent elevationExtent )
  {
    this.spatialExtent = spatialExtent;
    this.dimension = dimension;
    this.temporalExtent = temporalExtent;
    temporal = temporalExtent != null;
    this.elevationExtent = elevationExtent;
  }

  /**
   * return the dimension of the (spatial?) extent
   *  
   */
  public int getDimension()
  {
    return dimension;
  }

  /**
   * Lastly, an optional ElevationExtent element, lists the elevation intervals
   * or points at which coverages may be requested from a coverage layer.
   * <p>
   * ElevationExtent has an optional attribute uom indicating the units of
   * measure in which it expresses elevations.
   * <p>
   * ElevationExtent is intended to supplement a 2-dimensional spatial extent
   * with elevation information.
   *  
   */
  public ElevationExtent getElevationExtent()
  {
    return elevationExtent;
  }

  /**
   * First, a required SpatialExtent element lists the bounds along each of the
   * spatial dimensions within which coverages may be requested from a overage
   * layer.
   *  
   */
  public SpatialExtent getSpatialExtent()
  {
    return spatialExtent;
  }

  /**
   * An optional TemporalExtent element, depicted in Figure 6 below, lists the
   * times for which coverages may be requested from a coverage layer. These may
   * consist of one or more intervals or points in time.
   * <p>
   * TemporalExtent has an optional attribute uom indicating the units of
   * measure in which it expresses time intervals or instances.
   *  
   */
  public TemporalExtent getTemporalExtent()
  {
    return temporalExtent;
  }

  /**
   * returns true if the extent is temporal
   *  
   */
  public boolean isTemporal()
  {
    return temporal;
  }

}