/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.psiadapter.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.timeseries.wq.at.AtTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQPair;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.fifo.FifoCacheFactory;

/**
 * Implementation based on .at files and a .ini file. <br>
 * Also caches the already accessed tables.
 * 
 * @author Gernot Belger.
 */
public class AtWQProvider implements IWQProvider
{
  public final static SimpleDateFormat DF_AT_DICT = new SimpleDateFormat( "dd.mm.yyyy" );
  static
  {
    // Setting the timezone to UTC; we only read days so we want to have 0:0 as time value
    DF_AT_DICT.setTimeZone( TimeZone.getTimeZone( "UTC" ) );
  }

  /** The dictionary of at files: Strig -> Properties */
  private final Map m_atDictionary = new HashMap();

  private static URL s_atDictUrl;

  /** First-in-First-Out cache for at-content. 1 Minute, 100 objects */
  private static final Cache s_atCache = new FifoCacheFactory().newInstance( "atCache", 60 * 1000, 100 );

  public AtWQProvider( final String atDictLocation ) throws MalformedURLException
  {
    s_atDictUrl = new URL( atDictLocation );
    loadPreferences( m_atDictionary, s_atDictUrl );
  }

  /**
   * Determines if at based wq-tables are available for the given id.
   * 
   * @see org.kalypso.psiadapter.util.IWQProvider#hasWQTable(java.lang.String)
   */
  public boolean hasWQTable( final String objectId )
  {
    return m_atDictionary.containsKey( objectId );
  }

  /**
   * Returns a local wq-table for a given PSI object id. <br>
   * 
   * @param objectId
   *          Object id as obtained from PSICompact.ObjectInfo
   * @return A locally stored wq-table or <code>null</code> if no entry was found in the at-dictionary.
   * @see org.kalypso.psiadapter.util.IWQProvider#getWQTable(java.lang.String)
   */
  public WQTableSet getWQTable( final String objectId )
  {
    // If the cache contains the desired table, immediately return it
    final WQTableSet cachedTable = (WQTableSet)s_atCache.getObject( objectId );
    if( cachedTable != null )
      return cachedTable;

    // Else, find the dictionary and try to load it from there
    final WQTableSet tableSet = tableSetFromPreferences( objectId, m_atDictionary );
    s_atCache.addObject( objectId, tableSet );
    return tableSet;
  }

  /**
   * Reads a windows-ini like file into a Map <String, Properties> <br>
   * TODO: If we change to java5, we could use ini4j instead
   */
  private static void loadPreferences( final Map preferences, final URL location )
  {
    InputStream is = null;
    try
    {
      Pattern groupPattern = Pattern.compile( "\\[(.*)\\]" );

      is = location.openStream();

      final LineNumberReader lnr = new LineNumberReader( new InputStreamReader( is ) );
      String[] currentGroups = new String[]
      { "nogroup" };

      while( lnr.ready() )
      {
        final String line = lnr.readLine();
        if( line == null )
          break;

        if( line.length() == 0 || line.trim().charAt( 0 ) == '#' )
          continue;

        final Matcher matcher = groupPattern.matcher( line );
        if( matcher.matches() )
          currentGroups = matcher.group( 1 ).split( "," );
        else
        {
          final String[] strings = line.split( "=" );
          if( strings.length == 2 )
            addGroupEntry( preferences, currentGroups, strings[0].trim(), strings[1].trim() );
        }
      }

      is.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  /**
   * Helper method for loadPreferences
   */
  private static void addGroupEntry( final Map preferences, final String[] groups, final String key, final String value )
  {
    for( int i = 0; i < groups.length; i++ )
    {
      final String group = groups[i].trim();

      if( !preferences.containsKey( group ) )
        preferences.put( group, new Properties() );

      final Properties properties = (Properties)preferences.get( group );
      properties.put( key, value );
    }
  }

  private WQTableSet tableSetFromPreferences( final String objectId, final Map atDictionary )
  {
    if( !atDictionary.containsKey( objectId ) )
      return null;

    final Properties atNode = (Properties)atDictionary.get( objectId );
    final Set dates = atNode.keySet();
    final WQTable[] tables = new WQTable[dates.size()];
    String typeFrom = null;
    String typeTo = null;

    int i = 0;
    for( final Iterator iter = dates.iterator(); iter.hasNext(); )
    {
      final String dateString = (String)iter.next();
      final String atPath = atNode.getProperty( dateString, null );

      try
      {
        final Date validity = DF_AT_DICT.parse( dateString );

        final URL atUrl = new URL( s_atDictUrl, atPath ); // resolve pathes relative to the dictionary location

        final AtTable atTable = AtTable.readAt( atUrl );
        final WQPair[] values = atTable.getValues();
        final String tFrom = atTable.getTypeFrom();
        final String tTo = atTable.getTypeTo();

        // Check consistancy of timeseries types
        if( typeFrom == null )
          typeFrom = tFrom;
        else if( !typeFrom.equals( tFrom ) )
          throw new IllegalStateException( "Konvertierungstyp 'von' ist nur für alle Tafeln gleich: " + tFrom );
        if( typeTo == null )
          typeTo = tTo;
        else if( !typeTo.equals( tTo ) )
          throw new IllegalStateException( "Konvertierungstyp 'nach' ist nur für alle Tafeln gleich: " + tTo );

        tables[i++] = new WQTable( validity, 0, values );
      }
      catch( IllegalStateException e )
      {
        System.out.println( "Invalid data for psi-id '" + objectId + "': " + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
      catch( ParseException e )
      {
        System.out.println( "Invalid date '" + dateString + "' for psi-id '" + objectId + "': "
            + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
      catch( MalformedURLException e )
      {
        System.out.println( "Invalid path '" + atPath + "' to at file for psi-id '" + objectId + "': "
            + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
      catch( IOException e )
      {
        System.out.println( "Failed to read at-file '" + atPath + "' for psi-id '" + objectId + "': "
            + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
    }

    return new WQTableSet( tables, typeFrom, typeTo );
  }
}
