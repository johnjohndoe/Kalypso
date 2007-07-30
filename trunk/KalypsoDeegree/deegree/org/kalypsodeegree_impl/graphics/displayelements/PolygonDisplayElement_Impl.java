/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.io.Serializable;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.PolygonDisplayElement;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * DisplayElement for handling polygons
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class PolygonDisplayElement_Impl extends GeometryDisplayElement_Impl implements PolygonDisplayElement, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -2980154437699081214L;

  /**
   * Creates a new PolygonDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  protected PolygonDisplayElement_Impl( final Feature feature, final GM_Surface< ? >[] surfaces )
  {
    this( feature, surfaces, new PolygonSymbolizer_Impl() );
  }

  /**
   * Creates a new PolygonDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected PolygonDisplayElement_Impl( final Feature feature, final GM_Surface< ? >[] surfaces, final PolygonSymbolizer symbolizer )
  {
    super( feature, surfaces, symbolizer );
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection )
  {
    final Graphics2D g2 = (Graphics2D) g;

    final PolygonSymbolizer sym = (PolygonSymbolizer) getSymbolizer();
    final org.kalypsodeegree.graphics.sld.Fill fill = sym.getFill();
    final org.kalypsodeegree.graphics.sld.Stroke stroke = sym.getStroke();
    final UOM uom = sym.getUom();

    // no fill defined -> don't draw anything
    if( fill == null )
      return;

    final GM_Surface< ? >[] surfaces = (GM_Surface< ? >[]) getGeometry();

    try
    {
      final FillPolygonPainter painter = new FillPolygonPainter( fill, stroke, getFeature(), uom, projection );

      for( final GM_Surface< ? > element : surfaces )
      {
        painter.paintSurface( g2, element );
      }
    }
    catch( final FilterEvaluationException e )
    {
      Debug.debugException( e, "FilterEvaluationException caught evaluating an Expression!" );
    }
    catch( final Exception ex )
    {
      Debug.debugException( ex, "Exception caught evaluating an Expression!" );
    }
  }

}