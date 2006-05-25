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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.contribs.java.JavaApiContributionsExtension;
import org.kalypso.contribs.java.net.IUrlCatalog;

/**
 * The NSPrefixProvider is a singelton to ensures that namespaces are allways prefixed the same way. This is not
 * neccesary, but very nice.
 * <p>
 * Suggestion: Retrieve preffered namespaces via catalog mechanism, so new plugins can contribute new namesapce
 * prefixes.
 * </p>
 * 
 * @author doemming
 */
public class NSPrefixProvider
{
  private static NSPrefixProvider THE_NS_MAPPER = null;

  private IUrlCatalog m_catalog = null;

  private final HashMap<String, String> m_prefixMap = new HashMap<String, String>();

  public static NSPrefixProvider getInstance( )
  {
    if( THE_NS_MAPPER == null )
    {
      THE_NS_MAPPER = new NSPrefixProvider();
//      THE_NS_MAPPER.getPreferredPrefix( NS.XLINK, "xlink" );
      THE_NS_MAPPER.getPreferredPrefix( NS.XSD, "xs" );
      THE_NS_MAPPER.getPreferredPrefix( NS.XSD_SCHEMA, "xsd" );
      THE_NS_MAPPER.getPreferredPrefix( NS.KALYPSO_MAPVIEW, "mapv" );
      THE_NS_MAPPER.getPreferredPrefix( NS.KALYPSO_OBSVIEW, "obsv" );
//      THE_NS_MAPPER.getPreferredPrefix( NS.GML2, "gml2" );
      THE_NS_MAPPER.getPreferredPrefix( NS.KALYPSO_RRM, "rrm" );
      THE_NS_MAPPER.getPreferredPrefix( NS.KALYPSO_OBSLINK, "obslink" );
      THE_NS_MAPPER.getPreferredPrefix( NS.ADV, "adv" );

      // TODO add here all the well known namespaces...
    }
    return THE_NS_MAPPER;
  }

  private NSPrefixProvider( )
  {
    try
    {
      if( Platform.isRunning() )
          m_catalog = JavaApiContributionsExtension.getAllRegisteredCatalogs();
    }
    catch( final CoreException e )
    {
      KalypsoCommonsPlugin.getDefault().getLog().log( e.getStatus() );
    }
  }

  /**
   * @param namespaceUri
   * @param suggestion
   *          a suggestion for prefix or <code>null</code>
   * @return allways a valid prefix for given namespaceURI
   */
  public String getPreferredPrefix( final String namespaceURI, final String suggestion )
  {
    if( !m_prefixMap.containsKey( namespaceURI ) )
    {
      // if we have a catalog, use its prefix as suggestion
      if( m_catalog != null )
      {
        final String prefix = m_catalog.getPreferedNamespacePrefix( namespaceURI );
        if( prefix != null )
          applySuggestion( namespaceURI, prefix );
        else
          applySuggestion( namespaceURI, suggestion );
      }
      else
        applySuggestion( namespaceURI, suggestion );
    }
    return m_prefixMap.get( namespaceURI );
  }

  /** Puts the suggestion into the map, but ensures that it is unique. */
  private void applySuggestion( final String namespaceURI, String suggestion )
  {
    // test suggestion
    if( suggestion != null && suggestion.length() > 1 )
    {
      if( m_prefixMap.containsKey( suggestion ) ) // generate new key
        m_prefixMap.put( namespaceURI, generatePrefix( namespaceURI, 0 ) );
      else
        m_prefixMap.put( namespaceURI, suggestion );
    }
    else
      m_prefixMap.put( namespaceURI, generatePrefix( namespaceURI, 0 ) );
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
