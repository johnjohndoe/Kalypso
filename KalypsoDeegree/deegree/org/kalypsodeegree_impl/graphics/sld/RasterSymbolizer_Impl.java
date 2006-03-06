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

import java.awt.Color;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import net.opengis.sld.ObjectFactory;

import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.Interval;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.xml.Marshallable;

/**
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class RasterSymbolizer_Impl extends Symbolizer_Impl implements RasterSymbolizer, Marshallable
{

  private TreeMap m_colorMap = null;

  private Geometry m_geometry = null;

  private final int mode_intervalColorMapping = 0;

  private int mode = mode_intervalColorMapping;

  public RasterSymbolizer_Impl( TreeMap colorMap )
  {
    setColorMap( colorMap );
  }

  public TreeMap getColorMap( )
  {
    return m_colorMap;
  }

  public void setColorMap( TreeMap colorMap )
  {
    m_colorMap = colorMap;
  }

  public Geometry getGeometry( )
  {
    return m_geometry;
  }

  public void setGeometry( Geometry geometry )
  {
    m_geometry = geometry;
  }

  public int getMode( )
  {
    return mode;
  }

  public void setMode( int mode )
  {
    this.mode = mode;
  }

  public TreeMap getIntervalMap( )
  {
    TreeMap intervalMap = new TreeMap();
    Object[] colorMapKeys = m_colorMap.keySet().toArray();
    int startIndex = 0;
    double nullValue = -9999;
    if( ((Double) colorMapKeys[0]).doubleValue() == nullValue )
    {
      startIndex = 1;
    }
    for( int i = startIndex; i < colorMapKeys.length - 1; i++ )
    {
      ColorMapEntry colorMapEntry_i = (ColorMapEntry) m_colorMap.get( colorMapKeys[i] );
      ColorMapEntry colorMapEntry_i1 = (ColorMapEntry) m_colorMap.get( colorMapKeys[i + 1] );
      Interval interval = new Interval_Impl( colorMapEntry_i.getQuantity(), colorMapEntry_i1.getQuantity() );
      Color color = colorMapEntry_i.getColor();
      Color colorWithOpacity = new Color( color.getRed(), color.getGreen(), color.getBlue(), (int) Math.round( colorMapEntry_i.getOpacity() * 255 ) );
      intervalMap.put( interval, colorWithOpacity );
    }
    return intervalMap;
  }

  public String exportAsXML( )
  {
    try
    {
      final ObjectFactory fac = new ObjectFactory();
      final JAXBContext jc = JaxbUtilities.createQuiet( ObjectFactory.class );
      final Marshaller marshaller = JaxbUtilities.createMarshaller(jc);
      // marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE
      // );
      net.opengis.sld.RasterSymbolizer rasterSymbolizerElement = fac.createRasterSymbolizer();
      if( m_colorMap != null )
      {
        net.opengis.sld.ColorMap colorMapElement = fac.createColorMap();
        List colorMapEntryList = colorMapElement.getColorMapEntry();
        Iterator it = m_colorMap.keySet().iterator();
        while( it.hasNext() )
        {
          ColorMapEntry colorMapEntry = (ColorMapEntry) m_colorMap.get( it.next() );
          colorMapEntryList.add( colorMapEntry.exportAsXML() );
        }
        rasterSymbolizerElement.setColorMap( colorMapElement );
      }
      StringWriter writer = new StringWriter();
      marshaller.marshal( rasterSymbolizerElement, writer );
      writer.close();
      // System.out.println( writer.toString() );
      return ((writer.toString()).replaceFirst( "<?.*?>", "" )).trim();
    }
    catch( Exception e )
    {
      System.out.println( e );
      return null;
    }

  }

}