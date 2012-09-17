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

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import com.google.common.base.Charsets;

/**
 * @author Gernot Belger
 */
public class UpdateScript implements Comparable<UpdateScript>
{
  private static final String PROPERTY_TARGET_VERSION = "targetVersion"; //$NON-NLS-1$

  static final String PROPERTY_COMMENT = "comment"; //$NON-NLS-1$

  private static final String PROPERTY_LOCATION = "location"; //$NON-NLS-1$

  private static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  private static final String ELEMENT_PAGE = "page"; //$NON-NLS-1$

  private static final String PROPERTY_CLASS = "class"; //$NON-NLS-1$

  private final Version m_targetVersion;

  private final String m_location;

  private final String m_type;

  private final String m_bundleID;

  private final IConfigurationElement[] m_pageElements;

  public UpdateScript( final IConfigurationElement element )
  {
    m_targetVersion = new Version( element.getAttribute( PROPERTY_TARGET_VERSION ) );
    // m_comment = element.getAttribute( PROPERTY_COMMENT );
    m_location = element.getAttribute( PROPERTY_LOCATION );
    m_type = element.getAttribute( PROPERTY_TYPE );
    m_bundleID = element.getNamespaceIdentifier();

    m_pageElements = element.getChildren( ELEMENT_PAGE );
  }

  public Version getTargetVersion( )
  {
    return m_targetVersion;
  }

  public String[] loadSQL( ) throws IOException
  {
    final Bundle bundle = Platform.getBundle( m_bundleID );
    final URL entry = bundle.getEntry( m_location );
    final String rawSQL = IOUtils.toString( entry, Charsets.UTF_8.name() );

    final Collection<String> noComments = new ArrayList<>();

    final String[] lines = StringUtils.split( rawSQL, "\n\r" ); //$NON-NLS-1$
    for( final String line : lines )
    {
      final String trim = StringUtils.trimToEmpty( line );
      if( !trim.isEmpty() && !trim.startsWith( "--" ) ) //$NON-NLS-1$
      {
        noComments.add( trim );
      }
    }

    final String result = StringUtils.join( noComments, "\n" ); //$NON-NLS-1$

    return StringUtils.split( result, ';' ); //$NON-NLS-1$
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

  public IUpdateScriptPage[] createVariablePages( final UpdateScriptPageData data ) throws CoreException
  {
    final Collection<IUpdateScriptPage> pages = new ArrayList<>();

    for( final IConfigurationElement element : m_pageElements )
    {
      try
      {
        final IUpdateScriptPage page = (IUpdateScriptPage) element.createExecutableExtension( PROPERTY_CLASS );
        page.init( data );
        pages.add( page );
      }
      catch( final CoreException e )
      {
        throw e;
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Failed to create update wizard page", e ); //$NON-NLS-1$
        throw new CoreException( status );
      }
    }

    return pages.toArray( new IUpdateScriptPage[pages.size()] );
  }
}