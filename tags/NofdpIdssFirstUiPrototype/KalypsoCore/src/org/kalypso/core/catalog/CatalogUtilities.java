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
package org.kalypso.core.catalog;

import java.io.File;

import javax.xml.namespace.QName;

import org.apache.commons.lang.StringUtils;

/**
 * @author doemming
 */
public class CatalogUtilities
{
  /**
   * contract: a catalog URN is builded like this:<br>
   * baseURN="ogc:sld:"<br>
   * catalogsuffix="_catalog"<br>
   * resulting catalogURN="ogc:sld_catalog"
   */
  private static final String CATALOG_URN = "_catalog";

  public static String CATALOG_FILE_NAME = "catalog.xml";

  // TODO find out XML namespace
  public static final QName BASE = new QName( "xml", "base" );

  public static String getPathForCatalog( final String catalogURN )
  {
    if( !catalogURN.endsWith( ":" ) )
      throw new UnsupportedOperationException( "catalog uRN must end with ':' " + catalogURN );
    if( catalogURN.equals( ":" ) )
      return CATALOG_FILE_NAME;
    final String path = catalogURN.replace( ':', File.separator.charAt( 0 ) );
    if( path.endsWith( File.separator ) )
      return path + CATALOG_FILE_NAME;
    return path + File.separator + CATALOG_FILE_NAME;
  }

  /** 
   *   
   */
  public static String createCatalogURN( final String baseURN )
  {
    if( !baseURN.endsWith( ":" ) )
      throw new UnsupportedOperationException( "baseURN for catalog must end with suffix ':' " + baseURN );
    // replace last ':' with catalog suffix, that is the contract
    return baseURN.substring( 0, baseURN.length() - 1 ) + CATALOG_URN;
  }

  /**
   * get part of urn <br>
   * (e.g. "ogc:sld:gml" will return "ogc" for level 1 and "sld" for level 2 ...
   * 
   * @return part of urn
   */
  public static String getUrnSection( final String urn, int level )
  {
    if( level == 0 )
      return ":";
    try
    {
      final String[] urnParts = urn.split( ":" );
      return urnParts[level - 1];
    }
    catch( Exception e )
    {
      return null;
    }
  }

  /**
   * get max level of URN <br>
   * "urn" of "urn:" is 1<br>
   * "urn:ogc" or "urn:ogc:" is 2 <br>
   * "urn:ogc:sld" or "urn:ogc:sld:" is 3<br>
   * 
   * @return max level of URN
   */
  public static int getMaxLevel( final String urn )
  {
    if( urn == null || "".equals( urn ) || ":".equals( urn ) )
      return 0;
    final int result = StringUtils.countMatches( urn, ":" );
    if( urn.endsWith( ":" ) )
      return result;
    return result + 1;
  }

  /**
   * adds asection to URN
   */
  public static String addURNSection( final String baseURN, final String partURNToAppend )
  {
    if( baseURN == null || "".equals( baseURN ) || ":".equals( baseURN ) )
      return partURNToAppend;
    if( baseURN.endsWith( ":" ) )
      return baseURN + partURNToAppend;
    return baseURN + ":" + partURNToAppend;
  }

  /**
   * get a interval part of URN
   */
  public static String getUrnSection( final String urn, final int minLevel, final int maxLevel )
  {
    if( maxLevel < minLevel )
      throw new UnsupportedOperationException();
    final StringBuffer result = new StringBuffer();
    for( int level = minLevel; level <= maxLevel; level++ )
    {
      if( level > minLevel )
        result.append( ':' );
      final String urnSection = getUrnSection( urn, level );
      result.append( urnSection );
    }
    return result.toString();
  }
}
