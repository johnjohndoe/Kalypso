/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Area;

import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * @author Gernot Belger
 */
public class SurfacePaintPlainTriangleVisitor<T extends GM_SurfacePatch> implements ISurfacePatchVisitor<T>
{
  private static final PolygonSymbolizer m_defaultSymbolizer = new PolygonSymbolizer_Impl();

  private final Graphics m_gc;

  private final GeoTransform m_projection;

  private final IElevationColorModel m_colorModel;

  public SurfacePaintPlainTriangleVisitor( final Graphics gc, final GeoTransform projection, final IElevationColorModel colorModel )
  {
    m_gc = gc;
    m_projection = projection;
    m_colorModel = colorModel;
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitor#visit(org.kalypsodeegree.model.geometry.GM_SurfacePatch,
   *      double)
   */
  public boolean visit( final T patch, final double elevationSample ) throws Exception
  {
    if( patch instanceof GM_Triangle )
    {
      final GM_Triangle triangle = (GM_Triangle) patch;

      // TODO: create own paintTriangle method
      // TODO: either paint isoines or isoareas
      // TODO really split the patch along the isolines of the color-model
      paintThisSurface( patch, triangle.getExteriorRing()[0].getZ() );
    }
    else
      paintThisSurface( patch, elevationSample );

    return true;
  }

  private void paintThisSurface( final GM_SurfacePatch patch, final double elevation ) throws Exception
  {
    final Area area = calcTargetCoordinates( this.m_projection, patch );
    m_gc.setColor( m_colorModel.getColor( elevation ) );
    ((Graphics2D) m_gc).fill( area );
  }

  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  private Area calcTargetCoordinates( final GeoTransform projection, final GM_SurfacePatch patch ) throws Exception
  {
    final PolygonSymbolizer sym = m_defaultSymbolizer;
    final Stroke stroke = sym.getStroke();
    float width = 1;
    if( stroke != null )
    {
      width = (float) stroke.getWidth( null );
    }

    try
    {
      final GM_Position[] ex = patch.getExteriorRing();
      final GM_Position[][] inner = patch.getInteriorRings();

      final Area areaouter = SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, ex );
      if( inner != null )
      {
        for( final GM_Position[] innerRing : inner )
        {
          if( innerRing != null )
            areaouter.subtract( SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, innerRing ) );
        }
      }

      return areaouter;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

}
