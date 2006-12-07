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

import java.awt.Color;
import java.awt.image.DataBuffer;
import java.awt.image.SampleModel;
import java.util.Iterator;
import java.util.TreeMap;

import javax.media.jai.RasterFactory;

import org.kalypso.gis.doubleraster.DoubleRaster;
import org.kalypso.gis.doubleraster.DoubleRasterWalker;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Interval;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Creates a DataBuffer from the walked raster.
 * 
 * @author Dejan Antanaskovic and Gernot Belger
 */
public class DataBufferRasterWalker implements DoubleRasterWalker
{
  private final TreeMap m_intervalMap;

  private DataBuffer m_buffer;

  private final int m_mode;

  private int m_nCols;
  private int m_nRows;

  private SampleModel m_sampleModel;

  public DataBufferRasterWalker( final TreeMap intervalMap, final int mode )
  {
    m_intervalMap = intervalMap;
    m_mode = mode;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#start(org.kalypso.gis.doubleraster.DoubleRaster)
   */
  public void start( final DoubleRaster r )
  {
    m_nCols = r.getSizeX();
    m_nRows = r.getSizeY();
    final int nRows = r.getSizeY();

    m_sampleModel = RasterFactory.createBandedSampleModel( DataBuffer.TYPE_INT, m_nCols, nRows, 4 );
    m_buffer = m_sampleModel.createDataBuffer();
  }

  public SampleModel getSampleModel( )
  {
    return m_sampleModel;
  }
  
  public DataBuffer getBuffer( )
  {
    return m_buffer;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#afterLine(int)
   */
  public void afterLine( final int y )
  {
    // nothing to do
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#getResult()
   */
  public Object getResult( )
  {
    return getBuffer();
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  public void operate( final int x, final int y, final Coordinate c ) 
  {
    // HACK/REMARK: we should change the raster-walker implementation
    // maybe the -1 is necessary for the generalisation algorythm, so move the hack there
    if( y == -1 )
      return;
    
    Color actualColor = Color.WHITE;
    int alphaValue = 0;
    final double value = c.z;

    if( !Double.isNaN( value ) )
    {
      switch( m_mode )
      {
        case RasterDisplayElement_Impl.mode_intervalColorMapping:
        {
          // TODO: PERFORMANCE!!!
          Iterator it = m_intervalMap.keySet().iterator();
          while( it.hasNext() )
          {
            Interval interval = (Interval) it.next();
            if( interval.contains( value ) )
            {
              actualColor = (Color) m_intervalMap.get( interval );
              alphaValue = actualColor.getAlpha();
              break;
            }
          }
          break;
        }
        case RasterDisplayElement_Impl.mode_valueColorMapping:
        {
          if( m_intervalMap.containsKey( new Double( value ) ) )
          {
            ColorMapEntry colorMapEntry = (ColorMapEntry) m_intervalMap.get( new Double( value ) );
            actualColor = colorMapEntry.getColor();
            double opacity = colorMapEntry.getOpacity();
            alphaValue = (int) Math.round( opacity * 255 );
          }
          break;
        }
      }
    }
    else
    {
      double nullValue = -9999;
      if( m_intervalMap.containsKey( new Double( nullValue ) ) )
      {
        ColorMapEntry colorMapEntry = (ColorMapEntry) m_intervalMap.get( new Double( nullValue ) );
        actualColor = colorMapEntry.getColor();
        double opacity = colorMapEntry.getOpacity();
        alphaValue = (int) Math.round( opacity * 255 );
      }
      else
      {
        actualColor = Color.WHITE;
        alphaValue = 0;
      }
    }
    final int redValue = actualColor.getRed();
    final int greenValue = actualColor.getGreen();
    final int blueValue = actualColor.getBlue();
    final int elementNumber = x + y*m_nCols;
    m_buffer.setElem( 0, elementNumber, redValue );
    m_buffer.setElem( 1, elementNumber, greenValue );
    m_buffer.setElem( 2, elementNumber, blueValue );
    m_buffer.setElem( 3, elementNumber, alphaValue );
  }

}
