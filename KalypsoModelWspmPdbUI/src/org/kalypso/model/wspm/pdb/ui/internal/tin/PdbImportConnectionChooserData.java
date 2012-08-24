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
package org.kalypso.model.wspm.pdb.ui.internal.tin;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.ConnectionChooserData;

/**
 * @author Holger Albert
 */
public class PdbImportConnectionChooserData extends ConnectionChooserData
{
  public static final String PROPERTY_DEM_SERVER_PATH = "demServerPath"; //$NON-NLS-1$

  public static final String PROPERTY_DHM_INDEXES = "dhmIndexes"; //$NON-NLS-1$

  public static final String PROPERTY_DHM_INDEX = "dhmIndex"; //$NON-NLS-1$

  private IPath m_demServerPath;

  private DhmIndex[] m_dhmIndexes;

  private DhmIndex m_dhmIndex;

  public PdbImportConnectionChooserData( )
  {
    m_demServerPath = null;
    m_dhmIndexes = null;
    m_dhmIndex = null;
  }

  @Override
  public void setConnection( final IPdbConnection connection )
  {
    super.setConnection( connection );

    updateData( connection );
  }

  public void setDemServerPath( final IPath demServerPath )
  {
    final IPath oldValue = m_demServerPath;
    m_demServerPath = demServerPath;
    firePropertyChange( PROPERTY_DEM_SERVER_PATH, oldValue, m_demServerPath );
  }

  public IPath getDemServerPath( )
  {
    return m_demServerPath;
  }

  public void setDhmIndexes( final DhmIndex[] dhmIndexes )
  {
    final DhmIndex[] oldValue = m_dhmIndexes;
    m_dhmIndexes = dhmIndexes;
    firePropertyChange( PROPERTY_DHM_INDEXES, oldValue, m_dhmIndexes );
  }

  public DhmIndex[] getDhmIndexes( )
  {
    return m_dhmIndexes;
  }

  public void setDhmIndex( final DhmIndex dhmIndex )
  {
    final DhmIndex oldValue = m_dhmIndex;
    m_dhmIndex = dhmIndex;
    firePropertyChange( PROPERTY_DHM_INDEX, oldValue, m_dhmIndex );
  }

  public DhmIndex getDhmIndex( )
  {
    return m_dhmIndex;
  }

  private IStatus updateData( final IPdbConnection connection )
  {
    setDemServerPath( null );
    setDhmIndexes( null );
    setDhmIndex( null );

    if( connection == null )
      return Status.OK_STATUS;

    try
    {
      final Session session = connection.openSession();

      final PdbInfo info = new PdbInfo( session );
      final IPath demServerPath = info.getDemServerPath();
      final List<DhmIndex> dhmIndexes = GetPdbList.getList( session, DhmIndex.class );

      setDemServerPath( demServerPath );
      setDhmIndexes( dhmIndexes.toArray( new DhmIndex[] {} ) );
      setDhmIndex( null );

      return Status.OK_STATUS;
    }
    catch( final PdbConnectException ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Fehler beim Zugriff auf die Datenbank.", ex );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
  }
}