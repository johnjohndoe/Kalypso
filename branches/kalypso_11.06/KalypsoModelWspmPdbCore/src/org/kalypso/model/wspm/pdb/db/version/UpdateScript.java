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
package org.kalypso.model.wspm.pdb.db.version;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.java.net.UrlUtilities;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class UpdateScript implements Comparable<UpdateScript>
{
  private static final String PROPERTY_TARGET_VERSION = "targetVersion"; //$NON-NLS-1$

  static final String PROPERTY_COMMENT = "comment"; //$NON-NLS-1$

  private static final String PROPERTY_LOCATION = "location"; //$NON-NLS-1$

  private static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  private final Version m_targetVersion;

  private final String m_location;

  private final String m_type;

  private final String m_bundleID;

  public UpdateScript( final IConfigurationElement element )
  {
    m_targetVersion = new Version( element.getAttribute( PROPERTY_TARGET_VERSION ) );
    // m_comment = element.getAttribute( PROPERTY_COMMENT );
    m_location = element.getAttribute( PROPERTY_LOCATION );
    m_type = element.getAttribute( PROPERTY_TYPE );
    m_bundleID = element.getNamespaceIdentifier();
  }

  public Version getTargetVersion( )
  {
    return m_targetVersion;
  }

  public String[] loadSQL( ) throws IOException
  {
    final Bundle bundle = Platform.getBundle( m_bundleID );
    final URL entry = bundle.getEntry( m_location );
    // FIXME: encoding!
    final String rawSQL = UrlUtilities.toString( entry, "UTF-8" );

    final Collection<String> noComments = new ArrayList<String>();

    final String[] lines = StringUtils.split( rawSQL, "\n\r" ); //$NON-NLS-1$
    for( final String line : lines )
    {
      final String trim = StringUtils.trimToEmpty( line );
      if( !trim.isEmpty() && !trim.startsWith( "--" ) )
      {
        noComments.add( trim );
      }
    }

    final String result = StringUtils.join( noComments, "\n" );

    return StringUtils.split( result, ';' );
  }

  public String getType( )
  {
    return m_type;
  }

  @Override
  public int compareTo( final UpdateScript o )
  {
    return m_targetVersion.compareTo( o.getTargetVersion() );
  }
}