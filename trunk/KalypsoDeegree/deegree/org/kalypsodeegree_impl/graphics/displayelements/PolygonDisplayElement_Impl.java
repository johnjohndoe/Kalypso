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

import org.deegree.graphics.displayelements.PolygonDisplayElement;
import org.deegree.graphics.sld.GraphicFill;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.deegree_impl.tools.Debug;

/**
 * DisplayElement for handling polygons
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class PolygonDisplayElement_Impl extends GeometryDisplayElement_Impl implements
    PolygonDisplayElement, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -2980154437699081214L;

  /**
   * Creates a new PolygonDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  protected PolygonDisplayElement_Impl( Feature feature, GM_Surface geometry )
  {
    super( feature, geometry, null );

    Symbolizer defaultSymbolizer = new PolygonSymbolizer_Impl();
    this.setSymbolizer( defaultSymbolizer );
  }

  /**
   * Creates a new PolygonDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected PolygonDisplayElement_Impl( Feature feature, GM_Surface geometry,
      PolygonSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  /**
   * Creates a new PolygonDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  protected PolygonDisplayElement_Impl( Feature feature, GM_MultiSurface geometry )
  {
    super( feature, geometry, null );

    Symbolizer defaultSymbolizer = new PolygonSymbolizer_Impl();
    this.setSymbolizer( defaultSymbolizer );
  }

  /**
   * Creates a new PolygonDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected PolygonDisplayElement_Impl( Feature feature, GM_MultiSurface geometry,
      PolygonSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  public void paint( Graphics g, GeoTransform projection )
  {
    try
    {
      if( geometry == null )
        return;
      Area area = null;
      if( DEBUG_PaintEnv )
        paint( g, geometry.getEnvelope(), projection );
      if( geometry instanceof GM_Surface )
      {
        area = calcTargetCoordinates( projection, (GM_Surface)geometry );
        drawPolygon( g, area );
      }
      else
      {
        GM_MultiSurface msurface = (GM_MultiSurface)geometry;
        for( int i = 0; i < msurface.getSize(); i++ )
        {
          area = calcTargetCoordinates( projection, msurface.getSurfaceAt( i ) );
          drawPolygon( g, area );
        }
      }
    }
    catch( FilterEvaluationException e )
    {
      Debug.debugException( e, "FilterEvaluationException caught evaluating an Expression!" );
    }
    catch( Exception ex )
    {
      Debug.debugException( ex, "Exception caught evaluating an Expression!" );
    }
  }

  private double distance( double x1, double y1, double x2, double y2 )
  {
    return Math.sqrt( ( x2 - x1 ) * ( x2 - x1 ) + ( y2 - y1 ) * ( y2 - y1 ) );
  }

  /**
   * calculates the Area (image or screen coordinates) where to draw the
   * surface.
   */
  private Area calcTargetCoordinates( GeoTransform projection, GM_Surface surface )
      throws Exception
  {
    Area areaouter = null;

    PolygonSymbolizer sym = (PolygonSymbolizer)symbolizer;
    org.deegree.graphics.sld.Stroke stroke = sym.getStroke();
    float width = 1;
    if( stroke != null )
    {
      width = (float)stroke.getWidth( feature );
    }

    try
    {
      GM_SurfacePatch patch = surface.getSurfacePatchAt( 0 );
      GM_Position[] ex = patch.getExteriorRing();
      GM_Position[][] inner = patch.getInteriorRings();

      int[] x = new int[ex.length];
      int[] y = new int[ex.length];

      int k = 0;
      for( int i = 0; i < ex.length; i++ )
      {
        GM_Position position = projection.getDestPoint( ex[i] );
        int xx = (int)( position.getX() + 0.5 );
        int yy = (int)( position.getY() + 0.5 );

        if( k > 0 && k < ex.length - 1 )
        {
          if( distance( xx, yy, x[k - 1], y[k - 1] ) > width )
          {
            //if ( xx != x[k-1] || yy != y[k-1] ) {
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

      areaouter = new Area( new Polygon( x, y, k - 1 ) );
      if( inner != null )
      {
        for( int i = 0; i < inner.length; i++ )
        {
          if( inner[i] != null )
          {
            k = 0;
            x = new int[inner[i].length];
            y = new int[inner[i].length];
            for( int j = 0; j < inner[i].length; j++ )
            {
              GM_Position position = projection.getDestPoint( inner[i][j] );
              int xx = (int)( position.getX() + 0.5 );
              int yy = (int)( position.getY() + 0.5 );
              if( k > 0 && k < inner[i].length - 1 )
              {
                if( distance( xx, yy, x[k - 1], y[k - 1] ) > width )
                {
                  //if ( xx != x[k-1] || yy != y[k-1] ) {
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
            Area areainner = new Area( new Polygon( x, y, k - 1 ) );
            areaouter.subtract( areainner );
          }
        }
      }

    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
    }

    return areaouter;
  }

  /**
   * renders one surface to the submitted graphic context considering the also
   * submitted projection
   */
  private void drawPolygon( Graphics g, Area area ) throws FilterEvaluationException
  {
    Graphics2D g2 = (Graphics2D)g;

    PolygonSymbolizer sym = (PolygonSymbolizer)symbolizer;
    org.deegree.graphics.sld.Fill fill = sym.getFill();
    org.deegree.graphics.sld.Stroke stroke = sym.getStroke();

    // only draw filled polygon, if Fill-Element is given
    if( fill != null )
    {
      double opacity = fill.getOpacity( feature );

      // is completly transparent
      // if not fill polygon
      if( opacity > 0.01 )
      {
        Color color = fill.getFill( feature );
        int alpha = (int)Math.round( opacity * 255 );
        int red = color.getRed();
        int green = color.getGreen();
        int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );

        g2.setColor( color );

        GraphicFill gFill = fill.getGraphicFill();

        if( gFill != null )
        {
          BufferedImage texture = gFill.getGraphic().getAsImage( feature );
          Rectangle anchor = new Rectangle( 0, 0, texture.getWidth( null ), texture
              .getHeight( null ) );
          g2.setPaint( new TexturePaint( texture, anchor ) );
        }

        g2.fill( area );
      }
    }

    // only stroke outline, if Stroke-Element is given
    if( stroke != null )
    {
      double opacity = stroke.getOpacity( feature );

      if( opacity > 0.01 )
      {
        Color color = stroke.getStroke( feature );
        int alpha = (int)Math.round( opacity * 255 );
        int red = color.getRed();
        int green = color.getGreen();
        int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );

        g2.setColor( color );

        float[] dash = stroke.getDashArray( feature );

        // use a simple Stroke if dash == null or dash length < 2
        BasicStroke bs2 = null;
        float w = (float)stroke.getWidth( feature );

        if( ( dash == null ) || ( dash.length < 2 ) )
        {
          bs2 = new BasicStroke( w );
        }
        else
        {
          bs2 = new BasicStroke( w, stroke.getLineCap( feature ), stroke.getLineJoin( feature ),
              10.0f, dash, stroke.getDashOffset( feature ) );
        }

        g2.setStroke( bs2 );
        g2.draw( area );
      }
    }
  }
}