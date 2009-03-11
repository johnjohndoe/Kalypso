/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld;

import java.awt.BasicStroke;
import java.awt.Color;

import javax.xml.namespace.QName;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.LineAttributes;
import org.eclipse.swt.graphics.Pattern;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Resource;
import org.kalypso.gmlschema.EmptyGMLSchema;
import org.kalypso.gmlschema.feature.CustomFeatureType;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.GraphicFill;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * This is the basis of all symbolizers. It defines the method <tt>getGeometry</tt> that's common to all symbolizers.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class Symbolizer_Impl implements Symbolizer
{
  public enum UOM
  {
    pixel,
    meter,
    foot
  }

  public static Resource[] prepareGc( final GC gc, final Stroke stroke, final Feature feature ) throws FilterEvaluationException
  {
    final Color awtColor = stroke == null ? null : stroke.getStroke( feature );
    final RGB rgb = awtColor == null ? new RGB( 0, 0, 0 ) : new RGB( awtColor.getRed(), awtColor.getGreen(), awtColor.getBlue() );
    final org.eclipse.swt.graphics.Color color = new org.eclipse.swt.graphics.Color( gc.getDevice(), rgb );
    final int opacity = stroke == null ? 255 : (int) (stroke.getOpacity( feature ) * 255);

    gc.setForeground( color );
    gc.setAlpha( opacity );

    final float width = stroke == null ? 1 : (float) stroke.getWidth( feature );
    final LineAttributes lineAttributes = getLineAttributes( stroke, feature, width );

    gc.setLineAttributes( lineAttributes );

    return new Resource[] { color };
  }

  private static LineAttributes getLineAttributes( final Stroke stroke, final Feature feature, final float width ) throws FilterEvaluationException
  {
    if( stroke == null )
      return new LineAttributes( width );

    final float[] dash = stroke.getDashArray( feature );
    final int cap = capAwt2swt( stroke.getLineCap( feature ) );
    final int join = joinAwt2swt( stroke.getLineJoin( feature ) );
    final float dashOffset = stroke.getDashOffset( feature );

    if( dash == null || dash.length < 2 )
      return new LineAttributes( width, cap, join );

    return new LineAttributes( width, cap, join, SWT.LINE_DASH, dash, dashOffset, 10 );
  }

  public static Feature createPseudoFeature( )
  {
    final IFeatureType ft = new CustomFeatureType( new EmptyGMLSchema(), new QName( "", "" ), new IPropertyType[] {} );
    final Feature feature = FeatureFactory.createFeature( null, null, "legende", ft, false );
    return feature;
  }

  public static int joinAwt2swt( final int lineJoin )
  {
    switch( lineJoin )
    {
      case BasicStroke.JOIN_BEVEL:
        return SWT.JOIN_BEVEL;
      case BasicStroke.JOIN_MITER:
        return SWT.JOIN_MITER;
      case BasicStroke.JOIN_ROUND:
        return SWT.JOIN_ROUND;

      default:
        break;
    }
    throw new UnsupportedOperationException();
  }

  public static int capAwt2swt( final int lineCap )
  {
    switch( lineCap )
    {
      case BasicStroke.CAP_BUTT:
        return SWT.CAP_FLAT;
      case BasicStroke.CAP_ROUND:
        return SWT.CAP_ROUND;
      case BasicStroke.CAP_SQUARE:
        return SWT.CAP_SQUARE;
    }

    throw new UnsupportedOperationException();
  }

  public static Resource[] prepareGc( final GC gc, final org.kalypsodeegree.graphics.sld.Fill fill, final Feature feature ) throws FilterEvaluationException
  {
    final double opacity = fill == null ? 1.0 : fill.getOpacity( feature );
    final int alpha = (int) Math.round( opacity * 255 );

    final Color awtColor = fill == null ? null : fill.getFill( feature );
    final RGB rgb = awtColor == null ? new RGB( 128, 128, 128 ) : new RGB( awtColor.getRed(), awtColor.getGreen(), awtColor.getBlue() );
    final org.eclipse.swt.graphics.Color color = new org.eclipse.swt.graphics.Color( gc.getDevice(), rgb );
    gc.setAlpha( alpha );
    gc.setBackground( color );

    final GraphicFill gFill = fill == null ? null : fill.getGraphicFill();
    if( gFill != null )
    {
      final Image image = new Image( gc.getDevice(), gc.getClipping().width, gc.getClipping().height );
      final GC gc2 = new GC( image );
      gFill.getGraphic().paint( gc2, feature );

      final Pattern pattern = new Pattern( gc.getDevice(), image );
      gc.setBackgroundPattern( pattern );

      return new Resource[] { color, gc, image };
    }
    else
      return new Resource[] { color };
  }

  public static void disposeResource( final Resource[] resources )
  {
    if( resources == null )
      return;

    for( final Resource resource : resources )
      resource.dispose();
  }

  private double m_maxDenominator = 9E99;

  private double m_minDenominator = 0;

  private Geometry m_geometry = null;

  private UOM m_uom = UOM.pixel;

  /**
   * default constructor
   */
  Symbolizer_Impl( )
  {
    // geometry is null
  }

  /**
   * constructor initializing the class with the <Symbolizer>
   */
  Symbolizer_Impl( final Geometry geometry, final UOM uom )
  {
    setUom( uom );
    setGeometry( geometry );
  }

  /**
   * The Geometry element is optional and if it is absent then the default geometry property of the feature type that is
   * used in the containing FeatureStyleType is used. The precise meaning of default geometry property is
   * system-dependent. Most frequently, feature types will have only a single geometry property.
   * 
   * @return the geometry of the symbolizer
   */
  public Geometry getGeometry( )
  {
    return m_geometry;
  }

  /**
   * sets the <Geometry>
   * 
   * @param geometry
   *            the geometry of the symbolizer
   */
  public void setGeometry( final Geometry geometry )
  {
    this.m_geometry = geometry;
  }

  public void setUom( final UOM uom )
  {
    m_uom = uom;
  }

  public UOM getUom( )
  {
    return m_uom;
  }

  /**
   * @return the MinScaleDenominator
   */
  public double getMinScaleDenominator( )
  {
    return m_minDenominator;
  }

  /**
   * @param minDenominator
   *            the MinScaleDenominator
   */
  public void setMinScaleDenominator( final double minDenominator )
  {
    this.m_minDenominator = minDenominator;
  }

  /**
   * @return the MaxScaleDenominator
   */
  public double getMaxScaleDenominator( )
  {
    return m_maxDenominator;
  }

  /**
   * @param maxDenominator
   *            the MaxScaleDenominator
   */
  public void setMaxScaleDenominator( final double maxDenominator )
  {
    this.m_maxDenominator = maxDenominator;
  }

  /**
   * Default implementation, draw a black cross to indicate that here is something to do.
   * 
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#paintLegendGraphic(org.eclipse.swt.graphics.GC)
   */
  @SuppressWarnings("unused")
  public void paint( final GC gc, final Feature feature ) throws FilterEvaluationException
  {
    final Rectangle clipping = gc.getClipping();

    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );
    gc.setBackground( gc.getDevice().getSystemColor( SWT.COLOR_WHITE ) );
    gc.setLineAttributes( new LineAttributes( 1 ) );

    gc.drawLine( clipping.x, clipping.y, clipping.x + clipping.width, clipping.y + clipping.height );
    gc.drawLine( clipping.x, clipping.y + clipping.height, clipping.x + clipping.width, clipping.y );
  }
}