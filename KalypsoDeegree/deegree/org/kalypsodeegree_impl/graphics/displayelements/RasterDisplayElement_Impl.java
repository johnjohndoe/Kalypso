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
package org.deegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Image;

import org.deegree.graphics.displayelements.RasterDisplayElement;
import org.deegree.graphics.sld.RasterSymbolizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.graphics.sld.RasterSymbolizer_Impl;
import org.deegree_impl.model.gc.ImageGridCoverage;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.opengis.gc.GC_GridCoverage;
import org.opengis.pt.PT_Envelope;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class RasterDisplayElement_Impl extends DisplayElement_Impl implements RasterDisplayElement
{

  private RasterSymbolizer symbolizer = null;

  private GC_GridCoverage gc = null;

  /**
   * Creates a new RasterDisplayElement_Impl object.
   * 
   * @param gc
   *          raster
   */
  RasterDisplayElement_Impl( GC_GridCoverage gc )
  {
    setRaster( gc );
    symbolizer = new RasterSymbolizer_Impl();
  }

  /**
   * Creates a new RasterDisplayElement_Impl object.
   * 
   * @param gc
   *          raster
   * @param symbolizer
   */
  RasterDisplayElement_Impl( GC_GridCoverage gc, RasterSymbolizer symbolizer )
  {
    setRaster( gc );
    this.symbolizer = symbolizer;
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   *  
   */
  public void paint( Graphics g, GeoTransform projection )
  {
    try
    {
      PT_Envelope env = gc.getEnvelope();
      int minx = (int)( projection.getDestX( env.minCP.ord[0] ) + 0.5 );
      int maxy = (int)( projection.getDestY( env.minCP.ord[1] ) + 0.5 );
      int maxx = (int)( projection.getDestX( env.maxCP.ord[0] ) + 0.5 );
      int miny = (int)( projection.getDestY( env.maxCP.ord[1] ) + 0.5 );
      GM_Envelope bbox = GeometryFactory.createGM_Envelope( env.minCP.ord[0], env.minCP.ord[1],
          env.maxCP.ord[0], env.maxCP.ord[1] );
      if( gc instanceof ImageGridCoverage )
      {
        Image image = (Image)( (ImageGridCoverage)gc ).getRaster( bbox, maxx - minx, maxy - miny );

        g.drawImage( image, minx, miny, maxx - minx, maxy - miny, null );
      }
      else
      {
        //FIXME
        // handle other grid coverages
      }
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

  }

  /**
   * returns the content of the <tt>RasterDisplayElement</tt>
   * 
   * @return
   */
  public GC_GridCoverage getRaster()
  {
    return gc;
  }

  /**
   * sets the grid coverage that represents the content of the
   * <tt>RasterDisplayElement</tt>
   * 
   * @param gc
   *  
   */
  public void setRaster( GC_GridCoverage gc )
  {
    this.gc = gc;
  }

}