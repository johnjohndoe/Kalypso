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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.io.Serializable;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.PolygonDisplayElement;
import org.kalypsodeegree.graphics.sld.GraphicFill;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
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
  protected PolygonDisplayElement_Impl( final Feature feature, final GM_Surface[] surfaces )
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
  protected PolygonDisplayElement_Impl( final Feature feature, final GM_Surface[] surfaces, final PolygonSymbolizer symbolizer )
  {
    super( feature, surfaces, symbolizer );
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection )
  {
    try
    {
      final GM_Surface[] surfaces = (GM_Surface[]) getGeometry();
      for( final GM_Surface element : surfaces )
      {
        final Area area = calcTargetCoordinates( projection, element );
        drawPolygon( g, area, projection );
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

  private double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  private Area calcTargetCoordinates( final GeoTransform projection, final GM_Surface surface ) throws Exception
  {
    final PolygonSymbolizer sym = (PolygonSymbolizer) getSymbolizer();
    final Stroke stroke = sym.getStroke();
    float width = 1;
    if( stroke != null )
    {
      width = (float) stroke.getWidth( getFeature() );
    }

    try
    {
      final GM_SurfacePatch patch = surface.getSurfacePatchAt( 0 );
      final GM_Position[] ex = patch.getExteriorRing();
      final GM_Position[][] inner = patch.getInteriorRings();

      final Area areaouter = areaFromRing( projection, width, ex );
      if( inner != null )
      {
        for( final GM_Position[] innerRing : inner )
        {
          if( innerRing != null )
            areaouter.subtract( areaFromRing( projection, width, innerRing ) );
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

  private Area areaFromRing( final GeoTransform projection, final float width, final GM_Position[] ex )
  {
    final int[] x = new int[ex.length];
    final int[] y = new int[ex.length];

    int k = 0;
    for( final GM_Position element : ex )
    {
      final GM_Position position = projection.getDestPoint( element );
      final int xx = (int) (position.getX() + 0.5);
      final int yy = (int) (position.getY() + 0.5);

      if( k > 0 && k < ex.length - 1 )
      {
        if( distance( xx, yy, x[k - 1], y[k - 1] ) > width )
        {
          x[k] = xx;
          y[k] = yy;
          k++;
        }
      }
      else
      {
        x[k] = xx;
        y[k] = yy;
        k++;
      }
    }

    final Polygon polygon = new Polygon( x, y, k - 1 );
    return new Area( polygon );
  }

  /**
   * renders one surface to the submitted graphic context considering the also submitted projection
   */
  private void drawPolygon( final Graphics g, final Area area, final GeoTransform projection ) throws FilterEvaluationException
  {
    final Graphics2D g2 = (Graphics2D) g;

    final PolygonSymbolizer sym = (PolygonSymbolizer) getSymbolizer();
    final org.kalypsodeegree.graphics.sld.Fill fill = sym.getFill();
    final org.kalypsodeegree.graphics.sld.Stroke stroke = sym.getStroke();
    final UOM uom = sym.getUom();

    final Feature feature = getFeature();
    // only draw filled polygon, if Fill-Element is given
    if( fill != null )
    {
      final double opacity = fill.getOpacity( feature );

      // is completly transparent
      // if not fill polygon
      if( opacity > 0.01 )
      {
        Color color = fill.getFill( feature );
        final int alpha = (int) Math.round( opacity * 255 );
        final int red = color.getRed();
        final int green = color.getGreen();
        final int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );

        g2.setColor( color );

        final GraphicFill gFill = fill.getGraphicFill();

        if( gFill != null )
        {
          // TODO: rotation is not considered here
          final BufferedImage texture = gFill.getGraphic().getAsImage( feature, uom, projection );
          if( texture != null )
          {
            final Rectangle anchor = new Rectangle( 0, 0, texture.getWidth( null ), texture.getHeight( null ) );
            g2.setPaint( new TexturePaint( texture, anchor ) );
          }
        }

        try
        {
          g2.fill( area );
        }
        catch( final Exception e )
        {
          //  
        }
      }
    }

    // only stroke outline, if Stroke-Element is given
    if( stroke != null )
    {
      final double opacity = stroke.getOpacity( feature );
      if( opacity > 0.01 )
      {
        Color color = stroke.getStroke( feature );
        final int alpha = (int) Math.round( opacity * 255 );
        final int red = color.getRed();
        final int green = color.getGreen();
        final int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );

        g2.setColor( color );

        final float[] dash = stroke.getDashArray( feature );

        // use a simple Stroke if dash == null or dash length < 2
        BasicStroke bs2 = null;
        final float w = (float) stroke.getWidth( feature );

        if( (dash == null) || (dash.length < 2) )
        {
          bs2 = new BasicStroke( w );
        }
        else
        {
          bs2 = new BasicStroke( w, stroke.getLineCap( feature ), stroke.getLineJoin( feature ), 10.0f, dash, stroke.getDashOffset( feature ) );
        }

        g2.setStroke( bs2 );
        try
        {
          g2.draw( area );
        }
        catch( final Exception e )
        {
          //  
        }
      }
    }
  }
}