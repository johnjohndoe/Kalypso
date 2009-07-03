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
package org.kalypso.dwd;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * storage and loader of dwd raster format files
 * @author doemming
 */
public class RasterStorage
{
  public static boolean TEST_SCENARIO = false;
  
  private final String DATUM = "([0-9]{10})";

  private final String KEY = "([0-9]+)";

  private final String STUNDE = "([0-9]+)";

  /**
   * dateformat used in dwd raster format: "YYMMDDHHhh"
   */
  private static final SimpleDateFormat m_rasterDF = new SimpleDateFormat( "yyMMddHHmm" );

  private final Pattern HEADER_STATIC = Pattern.compile( " " + DATUM + " +" + KEY );

  private final Pattern HEADER_DYNAMIC = Pattern.compile( " " + DATUM + " +" + KEY + " +" + STUNDE );

  private final HashMap m_store;
  
  public RasterStorage()
  {
  m_store=new HashMap();  
  }
  
  public void loadRaster( LineNumberReader reader ) throws IOException, ParseException
  {
    String line = null;
    DWDRaster raster = null;
    while( ( line = reader.readLine() ) != null )
    {
      Matcher dynamicHeaderMatcher = HEADER_DYNAMIC.matcher( line );
      if( dynamicHeaderMatcher.matches() )
      {
        System.out.println( line );
        storeRaster( raster );
        final Date date = m_rasterDF.parse( dynamicHeaderMatcher.group( 1 ) );
        final int key = Integer.parseInt( dynamicHeaderMatcher.group( 2 ) );
        final long hour = Long.parseLong( dynamicHeaderMatcher.group( 3 ) );
        Date forecastDate = new Date( date.getTime() + 60 * 60 * 1000 * hour );
        raster = new DWDRaster( forecastDate, key );
        continue;
      }
      Matcher staticHeaderMatcher = HEADER_STATIC.matcher( line );
      if( staticHeaderMatcher.matches() )
      {
        System.out.println( line );
        storeRaster( raster );
        final Date date = m_rasterDF.parse( staticHeaderMatcher.group( 1 ) );
        final int key = Integer.parseInt( staticHeaderMatcher.group( 2 ) );
        raster = new DWDRaster( date, key );
        continue;

      }
      final String[] values;
      if( raster.getKey() == DWDRaster.KEY_RAIN && TEST_SCENARIO )
        values = line.trim().replaceAll( "[0-9]+", "100" ).split( " +", 13 );
      else
        values = ( line.trim() ).split( " +", 13 );

      if( raster != null )
      {
        raster.addValues( values );
      }
    }
    storeRaster( raster );
  }

  public Object get(int key)
  {
    final Integer storeKey = new Integer( key );
    return m_store.get(storeKey); 
  }
  
  public void storeRaster( DWDRaster raster )
  {
    if( raster == null )
      return;
    int key = raster.getKey();
    final Integer storeKey = new Integer( key );
    switch( key )
    {
    case DWDRaster.KEY_RAIN:
    case DWDRaster.KEY_SNOW:
      if( !m_store.containsKey( storeKey ) )
        m_store.put( storeKey, new ArrayList() );
      ( (List)m_store.get( storeKey ) ).add( raster );
      break;
    default:
      m_store.put( storeKey, raster );
      break;
    }
  }
  
  public void loadRaster( File rasterFile ) throws IOException, ParseException
  {
    final Reader reader = new FileReader( rasterFile );
    final LineNumberReader lineReader = new LineNumberReader( reader );
    loadRaster( lineReader );
    lineReader.close();
    reader.close();
  }  
}
