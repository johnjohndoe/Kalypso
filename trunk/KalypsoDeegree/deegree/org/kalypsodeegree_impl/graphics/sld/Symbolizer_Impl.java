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
package org.kalypsodeegree_impl.graphics.sld;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.LineAttributes;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.Symbolizer;

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
  public void paintLegendGraphic( final GC gc ) throws FilterEvaluationException
  {
    final Rectangle clipping = gc.getClipping();

    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );
    gc.setBackground( gc.getDevice().getSystemColor( SWT.COLOR_WHITE ) );
    gc.setLineAttributes( new LineAttributes( 1 ) );

    gc.drawLine( clipping.x, clipping.y, clipping.x + clipping.width, clipping.y + clipping.height );
    gc.drawLine( clipping.x, clipping.y + clipping.height, clipping.x + clipping.width, clipping.y );
  }
}