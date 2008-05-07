/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and Coastal Engineering
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
package org.kalypso.ogc.sensor.template;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * Utility methods for working with ObsView objects
 * 
 * @author belger
 * @author schlienger
 */
public class ObsViewUtils
{
  public static final String TOKEN_AXISUNIT = "%axisunit%"; //$NON-NLS-1$

  public static final String TOKEN_AXISTYPE = "%axistype%"; //$NON-NLS-1$

  public static final String TOKEN_AXISNAME = "%axisname%"; //$NON-NLS-1$

  public static final String TOKEN_OBSNAME = "%obsname%"; //$NON-NLS-1$

  public static final String DEFAULT_ITEM_NAME = "%axistype% - %obsname%"; //$NON-NLS-1$

  private ObsViewUtils( )
  {
    // utility class
  }

  /**
   * Replace tokens in Format-String
   * <dl>
   * <dt>%obsname%</dt>
   * <dd>Name der Observation: obs.getName()</dd>
   * <dt>%axisname%</dt>
   * <dd>Name der Wert-Achse: axis.getName()</dd>
   * <dt>%axistype%</dt>
   * <dd>Typ der Wert-Achse: axis.getType()</dd>
   * <dt>%axisunit%</dt>
   * <dd>Einheit der Wert-Achse: axis.getUnit()</dd>
   * </dl>
   */
  public static String replaceTokens( final String formatString, final IObservation obs, final IAxis axis )
  {
    String result = formatString;

    // observation
    if( obs != null )
      result = result.replaceAll( TOKEN_OBSNAME, obs.getName() );

    // axis
    if( axis != null )
    {
      result = result.replaceAll( TOKEN_AXISNAME, axis.getName() );
      result = result.replaceAll( TOKEN_AXISTYPE, axis.getType() );
      result = result.replaceAll( TOKEN_AXISUNIT, axis.getUnit() );
    }

    // Metadata
    if( obs != null )
    {
      int index = 0;
      while( index < result.length() - 1 )
      {
        final int start = result.indexOf( "%metadata-", index ); //$NON-NLS-1$
        if( start == -1 )
          break;

        final int stop = result.indexOf( '%', start + 1 );
        if( stop != -1 )
        {
          final String metaname = result.substring( start + "%metadata-".length(), stop ); //$NON-NLS-1$
          final StringBuffer sb = new StringBuffer( result );

          final String metaval = obs.getMetadataList().getProperty( metaname, "<Metavalue '" + metaname + "' not found>" ); //$NON-NLS-1$ //$NON-NLS-2$
          sb.replace( start, stop + 1, metaval );

          result = sb.toString();
        }

        index = stop + 1;
      }
    }

    return result;
  }

  /**
   * Replace the tokens found in formatString with the corresponding values from the mapping in the properties
   * <p>
   * TODO gibt es nicht schon sowas?
   */
  public static String replaceTokens( final String formatString, final Properties properties )
  {
    String result = formatString;

    for( Iterator it = properties.entrySet().iterator(); it.hasNext(); )
    {
      final Map.Entry entry = (Map.Entry) it.next();

      result = result.replaceAll( (String) entry.getKey(), (String) entry.getValue() );
    }

    return result;
  }

  /**
   * Replace the tokens found in formatString with the corresponding values from the mapping in the properties
   * <p>
   * TODO gibt es nicht schon sowas?
   * <p>
   * Die Liste der Tokens und deren Ersetzung in der Form:
   * <p>
   * tokenName-featurePropertyName;tokenName-featurePropertyName;...
   */
  public static String replaceTokens( final String formatString, final String tokens )
  {
    final Properties properties = new Properties();
    final String[] strings = tokens.split( ";" ); //$NON-NLS-1$
    for( int i = 0; i < strings.length; i++ )
    {
      final String[] splits = strings[i].split( "-" ); //$NON-NLS-1$
      properties.setProperty( splits[0], splits[1] );
    }

    return replaceTokens( formatString, properties );
  }

  // /**
  // * Remove the tokens so that name is clean.
  // *
  // * @param name
  // */
  // public static String removeTokens( final String name )
  // {
  // String res = name.replaceAll( TOKEN_AXISNAME, "" );
  // res = res.replaceAll( TOKEN_AXISTYPE, "" );
  // res = res.replaceAll( TOKEN_AXISUNIT, "" );
  // res = res.replaceAll( TOKEN_OBSNAME, "" );
  //
  // return res;
  // }
  //

  /**
   * Retrieve all axis-types of the observations associated to the ObsViewItems. Return a set so that there are no
   * duplicates.
   */
  public static Set<String> retrieveAxisTypes( final ObsViewItem[] items, final boolean onlyValueAxes )
  {
    final Set<String> set = new TreeSet<String>();

    if( items == null )
      return set;

    for( int i = 0; i < items.length; i++ )
    {
      final IAxis[] axes = items[i].getObservation().getAxisList();
      for( int j = 0; j < axes.length; j++ )
      {
        final String axisType = axes[j].getType();
        if( !onlyValueAxes || onlyValueAxes && !KalypsoStatusUtils.isStatusAxis( axes[j] ) && !TimeserieConstants.TYPE_DATE.equals( axisType ) )
          set.add( axisType );
      }
    }

    return set;
  }
}
