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
package org.kalypso.model.wspm.pdb.db;

import java.lang.reflect.Array;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.URIUtil;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.Info;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class PdbInfo
{
  public static final int UNKNOWN_SRID = -1;

  public static final Version CURRENT_VERSION = new Version( "12.11.0" ); //$NON-NLS-1$

  public final static String PROPERTY_VERSION = "Version"; //$NON-NLS-1$

  public final static String PROPERTY_SRID = "SRID"; //$NON-NLS-1$

  public final static String PROPERTY_DOCUMENT_SERVER = "DocumentServer"; //$NON-NLS-1$

  public final static String PROPERTY_DEM_SERVER = "DEMServer"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_MIN_X = "srsMinX"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_MAX_X = "srsMaxX"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_MIN_Y = "srsMinY"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_MAX_Y = "srsMaxY"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_X_NAME = "srsXName"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_Y_NAME = "srsYName"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_Z_NAME = "srsZName"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_MIN_Z = "srsMinZ"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_MAX_Z = "srsMaxZ"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_TOL_X = "srsTolX"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_TOL_Y = "srsTolY"; //$NON-NLS-1$

  public final static String PROPERTY_SRS_TOL_Z = "srsTolZ"; //$NON-NLS-1$

  private final Properties m_properties = new Properties();

  private IStatus m_status;

  public PdbInfo( final Session session )
  {
    // REAMRK: need to put this into a transaction, else, if an error occurs
    // later changes on the db do not work any more
    Transaction transaction = null;

    try
    {
      transaction = session.beginTransaction();

      final List<Info> list = GetPdbList.getList( session, Info.class );
      for( final Info property : list )
      {
        final String value = property.getValue();
        m_properties.put( property.getKey(), StringUtils.defaultString( value ) );
        m_status = Status.OK_STATUS;
      }
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      m_status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, Messages.getString( "PdbInfo.0" ), e ); //$NON-NLS-1$
    }
    finally
    {
      if( transaction != null )
        transaction.commit();
    }
  }

  public IStatus getStatus( )
  {
    return m_status;
  }

  public Version getVersion( )
  {
    final String version = m_properties.getProperty( PROPERTY_VERSION );
    if( StringUtils.isBlank( version ) )
      return null;

    return new Version( version );
  }

  public int getSRID( )
  {
    final String property = m_properties.getProperty( PROPERTY_SRID, Integer.toString( UNKNOWN_SRID ) );
    return NumberUtils.parseQuietInt( property, UNKNOWN_SRID );
  }

  /**
   * @return base part of document server's URL; Will be used to resolve document URLs.
   */
  public String getDocumentServer( )
  {
    return m_properties.getProperty( PROPERTY_DOCUMENT_SERVER );
  }

  public URI getDocumentBase( ) throws CoreException
  {
    final String documentServer = getDocumentServer();
    if( StringUtils.isBlank( documentServer ) )
    {
      final String message = String.format( Messages.getString( "PdbInfo.1" ) ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    try
    {
      final URI documentBase = new URI( documentServer );

      // BUGIFX: fixes the problem with file-uri's containing several slashes ('/') after the
      // first colon (e.g. file:///G:/). After resolving with another uri, the slashes are
      // always reduced to one (e.g. file:/G:/
      // This caused the problem, that the base uri and the document uri's did not start with
      // the same prefix, which lead to problems later.
      // We know directly resolve the base against the empty uri, to directly reduce it.
      return URIUtil.append( documentBase, StringUtils.EMPTY );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final String message = String.format( Messages.getString( "PdbInfo.2" ), documentServer ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, message, e );
      throw new CoreException( status );
    }
  }

  /**
   * @return base path to the server containing the dhm files.
   */
  public IPath getDemServerPath( ) throws CoreException
  {
    final String property = m_properties.getProperty( PROPERTY_DEM_SERVER );
    if( StringUtils.isBlank( property ) )
    {
      final String message = Messages.getString( "PdbInfo.3" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    return Path.fromPortableString( property );
  }

  public Entry<String, String>[] getEntries( )
  {
    final Collection<Entry<String, String>> entries = new ArrayList<>();

    final Set<String> names = m_properties.stringPropertyNames();
    for( final String name : names )
    {
      final String value = m_properties.getProperty( name );

      final Entry<String, String> entry = Collections.singletonMap( name, value ).entrySet().iterator().next();
      entries.add( entry );
    }

    return entries.toArray( (Entry<String, String>[])Array.newInstance( Entry.class, entries.size() ) );
  }
}