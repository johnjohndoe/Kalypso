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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;

/**
 * @author Gernot Belger
 */
public class UpdatePdbCrossSectionsOperation implements ICoreRunnableWithProgress
{
  private final Map<String, CrossSection> m_sectionHash = new HashMap<>();

  private final UpdatePdbCrossSectionsData m_data;

  public UpdatePdbCrossSectionsOperation( final UpdatePdbCrossSectionsData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    // fetch existing cross sections
    final IPdbConnection connection = m_data.getConnection();
    if( connection == null )
    {
      /* Happens in WspmLight perspective, if connection was not opened in database view */
      final String message = String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_0" ) ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    Session session = null;
    Transaction transaction = null;
    try
    {
      session = connection.openSession();

      /* Fetch the right state from database */
      final TuhhReach reach = m_data.getReach();
      final String stateName = reach.getName();
      final String queryString = String.format( "from State where name='%s'", stateName ); //$NON-NLS-1$
      final Query query = session.createQuery( queryString );
      final List< ? > list = query.list();
      if( list.size() == 0 )
      {
        final String message = String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_2" ), stateName ); //$NON-NLS-1$
        return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, message );
      }

      final State state = (State) list.get( 0 );

      /* Update data */
      transaction = session.beginTransaction();

      /* Load and hash sections from db */
      final Set<CrossSection> existingCrossSections = state.getCrossSections();
      buildSectionsHash( existingCrossSections );

      /* write data from local sections into db sections */
      final IStatusCollector log = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );
      final IProfileFeature[] sections = m_data.getSelectedProfiles();
      for( final IProfileFeature profileFeature : sections )
        log.add( updateRemoteInstance( profileFeature ) );

      transaction.commit();

      session.close();

      return log.asMultiStatus( Messages.getString( "UpdatePdbCrossSectionsOperation_3" ) ); //$NON-NLS-1$
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final String message = String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_4" ) ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, message, e );
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();

      if( transaction != null && transaction.isActive() )
      {
        try
        {
          transaction.rollback();
        }
        catch( final HibernateException e1 )
        {
          // TODO: error handling...
          e1.printStackTrace();
        }
      }

      final String message = String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_5" ) ); //$NON-NLS-1$
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, message, e );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  private IStatus updateRemoteInstance( final IProfileFeature profileFeature ) throws HibernateException
  {
    final String profileLabel = String.format( "%.4f", profileFeature.getBigStation() ); //$NON-NLS-1$
    final String name = profileFeature.getName();

    final CrossSection section = m_sectionHash.get( name );
    if( section == null )
    {
      final String message = String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_7" ), profileLabel, name ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    boolean changed = false;

    if( m_data.getUpdateComments() )
    {
      changed |= !ObjectUtils.equals( section.getDescription(), profileFeature.getDescription() );
      section.setDescription( profileFeature.getDescription() );
    }

    // session.persist( section );

    if( changed )
      return new Status( IStatus.INFO, WspmPdbUiPlugin.PLUGIN_ID, String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_8" ), profileLabel ) ); //$NON-NLS-1$
    else
      return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, String.format( Messages.getString( "UpdatePdbCrossSectionsOperation_9" ), profileLabel ) ); //$NON-NLS-1$
  }

  private void buildSectionsHash( final Collection<CrossSection> existingCrossSections )
  {
    for( final CrossSection crossSection : existingCrossSections )
      m_sectionHash.put( crossSection.getName(), crossSection );
  }
}