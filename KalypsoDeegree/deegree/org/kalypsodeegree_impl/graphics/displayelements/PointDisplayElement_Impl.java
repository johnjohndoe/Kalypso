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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.Serializable;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.PointDisplayElement;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiPrimitive;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Primitive;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * DisplayElement that encapsulates a point geometry (<tt>GM_Point</tt>) and a <tt>PointSymbolizer</tt>.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
class PointDisplayElement_Impl extends GeometryDisplayElement_Impl implements PointDisplayElement, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -2979559276151855757L;

  private transient static Image defaultImg = new BufferedImage( 7, 7, BufferedImage.TYPE_INT_ARGB );

  static
  {
    final Graphics g = defaultImg.getGraphics();
    g.setColor( Color.LIGHT_GRAY );
    g.fillRect( 0, 0, 9, 9 );
    g.dispose();
  }

  /**
   * Creates a new PointDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  PointDisplayElement_Impl( final Feature feature, final GM_Point geometry )
  {
    super( feature, geometry, null );

    final Symbolizer defaultSymbolizer = new PointSymbolizer_Impl();
    this.setSymbolizer( defaultSymbolizer );
  }

  /**
   * Creates a new PointDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  PointDisplayElement_Impl( final Feature feature, final GM_Point geometry, final PointSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  /**
   * Creates a new PointDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  PointDisplayElement_Impl( final Feature feature, final GM_MultiPoint geometry )
  {
    super( feature, geometry, null );

    final Symbolizer defaultSymbolizer = new PointSymbolizer_Impl();
    this.setSymbolizer( defaultSymbolizer );
  }

  /**
   * Creates a new PointDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  PointDisplayElement_Impl( final Feature feature, final GM_MultiPoint geometry, final PointSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection )
  {
    try
    {
      final PointSymbolizer symbolizer = (PointSymbolizer) getSymbolizer();
      final UOM uom = symbolizer.getUom();
      final Graphic graphic = symbolizer.getGraphic();
      final Feature feature = getFeature();

      final Image image;
      final double rotation;
      if( graphic == null )
      {
        image = defaultImg;
        rotation = Graphic.ROTATION_DEFAULT;
      }
      else
      {
        image = graphic.getAsImage( feature, uom, projection );
        rotation = graphic.getRotation( feature );
      }

      final Graphics2D g2D = (Graphics2D) g;

      final GM_Object geometry = getGeometry();
      if( geometry instanceof GM_MultiPrimitive )
      {
        final GM_Primitive[] primitives = ((GM_MultiPrimitive) geometry).getAllPrimitives();
        for( final GM_Primitive primitive : primitives )
          drawPoint( g2D, primitive.getCentroid(), projection, image, rotation );
      }
      else
        drawPoint( g2D, geometry.getCentroid(), projection, image, rotation );
    }
    catch( final FilterEvaluationException e )
    {
      Debug.debugException( e, "Exception caught evaluating an Expression!" );
    }
  }

  /**
   * renders one point to the submitted graphic context considering the also submitted projection
   */
  private void drawPoint( final Graphics2D g, final GM_Point point, final GeoTransform projection, final Image image, final double rotation )
  {
    final GM_Position source = point.getPosition();
    final int x = (int) (projection.getDestX( source.getX() ) + 0.5);
    final int y = (int) (projection.getDestY( source.getY() ) + 0.5);

    final int x_ = x - (image.getWidth( null ) >> 1);
    final int y_ = y - (image.getHeight( null ) >> 1);

    g.rotate( Math.toRadians( rotation ), x, y );

    g.drawImage( image, x_, y_, null );

    g.rotate( -Math.toRadians( rotation ), x, y );
  }
}