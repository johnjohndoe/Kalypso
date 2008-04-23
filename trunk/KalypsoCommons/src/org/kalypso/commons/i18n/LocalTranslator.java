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
package org.kalypso.commons.i18n;

import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.w3c.dom.Element;

/**
 * A translator based on properties files, that are found relative to the given context.
 * 
 * @see org.eclipse.osgi.util.NLS
 * @author Gernot Belger
 */
public class LocalTranslator implements ITranslator, IExecutableExtension
{
  private final Map<Locale, Properties> m_properties = new HashMap<Locale, Properties>();

  private String m_id;

  private List<Element> m_configuration;

  private String m_path;

  private URL m_context;

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_id = config.getAttribute( "id" );
  }

  /**
   * @see org.kalypso.contribs.java.lang.I10nTranslator#getId()
   */
  public String getId( )
  {
    return m_id;
  }

  /**
   * @see org.kalypso.contribs.java.lang.I10nTranslator#configure(java.util.List)
   */
  @SuppressWarnings("unchecked")
  public void configure( final URL context, final List<Element> configuration )
  {
    m_context = context;
    m_configuration = configuration;

    for( final Element element : configuration )
    {
      m_path = element.getTextContent();
      return;
    }

    // If we reach this line, no path was configured: we try to use the context itself instead:
    final String externalForm = m_context.toExternalForm();
    m_path = FileUtilities.nameWithoutExtension( externalForm );
  }

  /**
   * @see org.kalypso.contribs.java.lang.I10nTranslator#getConfiguration()
   */
  public List<Element> getConfiguration( )
  {
    return m_configuration;
  }

  /**
   * REMARK: locale is always ignored, as the language is determined whn the message class is loaded. It is always the
   * current locale of the eclipse platform.
   * 
   * @see org.kalypso.contribs.java.lang.I10nTranslator#get(java.lang.String, java.util.Locale, java.lang.Object[])
   */
  public String get( final String key, final Locale locale, final Object[] context )
  {
    if( m_path == null )
      return "Key: " + key;

    final Properties props = getProperties( locale );
    return props.getProperty( key, "Key not found: " + key );
  }

  private Properties getProperties( final Locale locale )
  {
    if( m_properties.containsKey( locale ) )
      return m_properties.get( locale );

    final Properties properties = new Properties();
    m_properties.put( locale, properties );

    // try to load the properties in order of suffixes
    PropertiesUtilities.loadI18nProperties( properties, m_context, m_path );

    return properties;
  }
}
