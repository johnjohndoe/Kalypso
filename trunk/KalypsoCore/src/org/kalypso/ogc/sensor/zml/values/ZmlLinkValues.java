/*--------------- Kalypso-Header --------------------------------------------------------------------

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
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.zml.values;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Map;
import java.util.regex.Pattern;

import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.io.CSV;
import org.kalypso.util.io.ITabledValues;
import org.kalypso.util.io.RegexCSV;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.AxisType.ValueLinkType;

/**
 * @author schlienger
 */
public class ZmlLinkValues implements IZmlValues
{
  private final ITabledValues m_csv;

  private final IParser m_parser;

  private Map m_helper = new Hashtable();

  private final int m_column;

  public ZmlLinkValues( final ValueLinkType vl, final IParser parser, final URL context )
      throws MalformedURLException, IOException
  {
    m_parser = parser;

    // index begins with 0 internally
    m_column = vl.getColumn() - 1;

    final URL url = new UrlUtilities().resolveURL( context, vl.getHref() );

    if( vl.getRegexp() == null || vl.getRegexp().length() == 0 )
    {
      // stream is closed in CSV()
      m_csv = new CSV( new InputStreamReader( url.openStream() ), vl.getSeparator(), vl.getLine(), true );
    }
    else
    {
      // stream closed in RegexCSV()
      m_csv = new RegexCSV( new InputStreamReader( url.openStream() ), Pattern.compile( vl
          .getRegexp() ), vl.getLine(), true );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getElement(int)
   */
  public Object getElement( int index ) throws SensorException
  {
    try
    {
      // get item from csv file
      final String item = m_csv.getItem( index, m_column );

      if( item.length() == 0 )
      {
        System.out.println( "empty element" );
      }
      
      // parse item using axis parser
      final Object obj = m_parser.parse( item );

      // tricky: store relation between element and index for future needs
      m_helper.put( obj, new Integer( index ) );

      return obj;
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#setElement(int,
   *      java.lang.Object)
   */
  public void setElement( int index, Object element ) throws SensorException
  {
    // tricky: set it in our map-helper (Siehe this.indexOf() )
    m_helper.put( element, new Integer( index ) );

    try
    {
      // set it in CSV
      m_csv.setItem( index, m_column, m_parser.toString( element ) );
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getCount()
   */
  public int getCount()
  {
    return m_csv.getLines();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#indexOf(java.lang.Object)
   */
  public int indexOf( final Object obj )
  {
    Integer iobj = (Integer)m_helper.get( obj );
    if( iobj == null )
      return -1;

    return iobj.intValue();
  }
}