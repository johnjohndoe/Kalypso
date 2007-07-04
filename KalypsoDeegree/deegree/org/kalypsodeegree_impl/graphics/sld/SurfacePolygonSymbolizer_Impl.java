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
package org.kalypsodeegree_impl.graphics.sld;

import org.eclipse.swt.graphics.GC;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;

/**
 * @author Thomas Jung
 */
public class SurfacePolygonSymbolizer_Impl extends Symbolizer_Impl implements SurfacePolygonSymbolizer, Marshallable
{
  private PolygonColorMap m_colorMap;

  /**
   * Creates a new PolygonSymbolizer_Impl object.
   */
  public SurfacePolygonSymbolizer_Impl( )
  {
    this( new PolygonColorMap_Impl(), null, 0, 99E9, UOM.pixel );
  }

  /**
   * constructor initializing the class with the <PolygonSymbolizer>
   */
  public SurfacePolygonSymbolizer_Impl( PolygonColorMap colorMap, final Geometry geometry, final double min, final double max, final UOM uom )
  {
    super( geometry, uom );
    setColorMap( colorMap );
    setMinScaleDenominator( min );
    setMaxScaleDenominator( max );
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer#getColorMap()
   */
  public PolygonColorMap getColorMap( )
  {
    return m_colorMap;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer#setColorMap(org.kalypsodeegree_impl.graphics.sld.PolygonColorMap)
   */
  public void setColorMap( PolygonColorMap colorMap )
  {
    m_colorMap = colorMap;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#getGeometry()
   */
  public Geometry getGeometry( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#getMaxScaleDenominator()
   */
  public double getMaxScaleDenominator( )
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#getMinScaleDenominator()
   */
  public double getMinScaleDenominator( )
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#getUom()
   */
  public UOM getUom( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#paint(org.eclipse.swt.graphics.GC,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  public void paint( GC gc, Feature feature ) throws FilterEvaluationException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#setGeometry(org.kalypsodeegree.graphics.sld.Geometry)
   */
  public void setGeometry( Geometry geometry )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#setMaxScaleDenominator(double)
   */
  public void setMaxScaleDenominator( double maxScaleDenominator )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#setMinScaleDenominator(double)
   */
  public void setMinScaleDenominator( double minScaleDenominator )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Symbolizer#setUom(org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM)
   */
  public void setUom( UOM uom )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypsodeegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML( )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
