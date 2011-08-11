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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.Info;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class PdbInfo
{
  public static final int UNKNOWN_SRID = -1;

  public static final Version CURRENT_VERSION = new Version( "0.0.3" ); //$NON-NLS-1$

  public final static String PROPERTY_VERSION = "Version"; //$NON-NLS-1$

  public final static String PROPERTY_SRID = "SRID"; //$NON-NLS-1$

  public final static String PROPERTY_DOCUMENT_SERVER = "DocumentServer"; //$NON-NLS-1$

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
      m_status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, "Failed to load 'Info' table from database. Database might not exist yet.", e );
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

  @SuppressWarnings("unchecked")
  public Entry<String, String>[] getEntries( )
  {
    final Collection<Entry<String, String>> entries = new ArrayList<Entry<String, String>>();

    final Set<String> names = m_properties.stringPropertyNames();
    for( final String name : names )
    {
      final String value = m_properties.getProperty( name );

      final Entry<String, String> entry = Collections.singletonMap( name, value ).entrySet().iterator().next();
      entries.add( entry );
    }

    return entries.toArray( (Entry<String, String>[]) Array.newInstance( Entry.class, entries.size() ) );
  }
}