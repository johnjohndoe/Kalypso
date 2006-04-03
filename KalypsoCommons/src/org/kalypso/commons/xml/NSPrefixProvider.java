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
package org.kalypso.commons.xml;

import java.util.HashMap;

/**
 * The NSPrefixProvider is a singelton to ensures that namespaces are allways prefixed the same way. This is not
 * neccesary, but very nice.
 * 
 * @author doemming
 */
public class NSPrefixProvider
{
  private static NSPrefixProvider THE_NS_MAPPER = null;

  final HashMap<String, String> m_prefixMap = new HashMap<String, String>();

  public static NSPrefixProvider getInstance( )
  {
    if( THE_NS_MAPPER == null )
    {
      THE_NS_MAPPER = new NSPrefixProvider();
      THE_NS_MAPPER.getPreferredPrefix( NS.XLINK, "xlink" );
      THE_NS_MAPPER.getPreferredPrefix( NS.NS_XSD, "xs" );
      THE_NS_MAPPER.getPreferredPrefix( NS.NS_XSD_SCHEMA, "xsd" );
      THE_NS_MAPPER.getPreferredPrefix( NS.NS_MAPVIEW, "mapv" );
      THE_NS_MAPPER.getPreferredPrefix( NS.GML2, "gml2" );
      THE_NS_MAPPER.getPreferredPrefix( NS.KALYPSO_RRM, "rrm" );
      THE_NS_MAPPER.getPreferredPrefix( NS.KALYPSO_OBSLINK, "obslink" );
      THE_NS_MAPPER.getPreferredPrefix( NS.NS_ADV, "adv" );

      // TODO add here all the well known namespaces...
    }
    return THE_NS_MAPPER;
  }

  private NSPrefixProvider( )
  {
  }

  /**
   * @param namespaceUri
   * @param suggestion
   *          a suggestion for prefix or <code>null</code>
   * @return allways a valid prefix for given namespaceURI
   */
  public String getPreferredPrefix( final String namespaceURI, final String suggestion )
  {
    if( !(m_prefixMap.containsKey( namespaceURI )) )
    {
      // test suggestion
      if( suggestion != null && suggestion.length() > 1 )
      {
        if( m_prefixMap.containsKey( suggestion ) ) // generate new key
        {
          m_prefixMap.put( namespaceURI, generatePrefix( namespaceURI, 0 ) );
        }
        else
          m_prefixMap.put( namespaceURI, suggestion );
      }
      else
        m_prefixMap.put( namespaceURI, generatePrefix( namespaceURI, 0 ) );
    }
    return m_prefixMap.get( namespaceURI );
  }

  private String generatePrefix( final String namespaceUri, int tryIndex )
  {
    // TODO better methodes to generate prefix
    switch( tryIndex )
    {
      // case 0:
      // {
      // final String ns = StringUtils.abbreviate( namespaceUri, 5 );
      // if( isValidPrefix( ns ) )
      // return ns;
      // else
      // return generatePrefix( namespaceUri, ++tryIndex );
      // }
      default:
      {
        int index = 0;
        String ns;
        do
        {
          ns = "ns" + index;
        }
        while( !isValidPrefix( ns ) );
        return ns;
      }
    }
  }

  private boolean isValidPrefix( String ns )
  {
    if( ns == null )
      return false;
    if( ns.indexOf( " " ) >= 0 ) // TODO better check
      return false;
    return !m_prefixMap.containsKey( ns );
  }

}
