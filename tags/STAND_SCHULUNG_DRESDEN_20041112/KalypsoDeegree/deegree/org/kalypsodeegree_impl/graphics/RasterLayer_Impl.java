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
package org.deegree_impl.graphics;

import org.deegree.graphics.RasterLayer;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.gc.GC_GridCoverage;

/**
 * A <tt>RasterLayer</tt> represent a layer which data are contained within
 * one single <tt>Image</tt>. The image/raster is geo-referenced by a
 * <tt>GM_Envelope</tt> that is linked to it.
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class RasterLayer_Impl extends Layer_Impl implements RasterLayer
{

  protected GC_GridCoverage raster = null;

  /**
   * Creates a new Layer_Impl object.
   * 
   * @param name
   * @param raster
   * 
   * @throws Exception
   */
  RasterLayer_Impl( String name, GC_GridCoverage raster ) throws Exception
  {
    super( name, raster.getCoordinateSystem() );
    setRaster( raster );
  }

  /**
   * sets the coordinate reference system of the MapView. If a new crs is set
   * all geometries of GeometryFeatures will be transformed to the new
   * coordinate reference system.
   */
  public void setCoordinatesSystem( CS_CoordinateSystem crs ) throws Exception
  {
  //throw new NoSuchMethodError( "not implemented yet" );
  }

  /**
   * returns the image/raster that represents the layers data
   *  
   */
  public GC_GridCoverage getRaster()
  {
    return raster;
  }

  /**
   * sets the image/raster that represents the layers data
   *  
   */
  public void setRaster( GC_GridCoverage raster ) throws Exception
  {
    this.raster = raster;
  }

}