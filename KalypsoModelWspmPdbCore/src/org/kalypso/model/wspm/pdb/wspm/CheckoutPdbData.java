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
package org.kalypso.model.wspm.pdb.wspm;

import java.net.URI;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetCoefficients;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbData extends AbstractModelObject
{
  /** Choice what shall be removed from the local data. */
  public enum RemoveStrategy
  {
    keepAll( Messages.getString( "CheckoutPdbData.0" ) ), //$NON-NLS-1$
    keepWaterBodies( Messages.getString( "CheckoutPdbData.1" ) ), //$NON-NLS-1$
    removeAll( Messages.getString( "CheckoutPdbData.2" ) ); //$NON-NLS-1$

    private final String m_label;

    private RemoveStrategy( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  public static final String PROPERTY_CONFIRM_EXISTING = "confirmExisting"; //$NON-NLS-1$

  public static final String PROPERTY_REMOVE_STRATEGY = "removeStrategy"; //$NON-NLS-1$

  private RemoveStrategy m_removeStrategy = RemoveStrategy.keepAll;

  private CheckoutDataMapping m_mapping;

  private boolean m_confirmExisting = false;

  private URI m_documentBase;

  private IPdbConnection m_connection;

  private ICoefficients m_coefficients;

  private GafCodes m_codes;

  private Session m_session;

  private int m_dbSRID;

  public IStatus init( final Shell shell, final String windowTitle, final IPdbConnection connection )
  {
    try
    {
      closeConnection();

      m_connection = connection;
      m_documentBase = findDocumentBase( shell, windowTitle, connection );

      m_session = connection.openSession();

      m_dbSRID = connection.getInfo().getSRID();

      m_coefficients = loadCoefficients( m_session );

      m_codes = new GafCodes();

      // REMAK: we do NOT close the session, because sub-elemnts will be accessed during checkout. Session will be closed when wizard closes.
      // session.close();

      return Status.OK_STATUS;
    }
    catch( final HibernateException e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
    catch( final PdbConnectException e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
  }

  /**
   * Fetches the selected elements from the new session because a) the old session is closed and the objects cannot be accessed any more, b) we want to be sure to have current elements.
   */
  private Object[] refetchElements( final Session session, final Object[] elements )
  {
    final Object[] updatedElements = new Object[elements.length];

    for( int i = 0; i < updatedElements.length; i++ )
      updatedElements[i] = refetchElement( session, elements[i] );

    return updatedElements;
  }

  private Object refetchElement( final Session session, final Object element )
  {
    if( element instanceof WaterBody )
      return session.load( WaterBody.class, ((WaterBody)element).getId() );

    if( element instanceof State )
      return session.load( State.class, ((State)element).getId() );

    if( element instanceof Event )
      return session.load( Event.class, ((Event)element).getId() );

    throw new IllegalArgumentException();
  }

  private ICoefficients loadCoefficients( final Session session ) throws PdbConnectException
  {
    final GetCoefficients operation = new GetCoefficients( IGafConstants.POINT_KIND_GAF );
    operation.execute( session );
    return operation.getCoefficients();
  }

  public void store( final IDialogSettings settings )
  {
    if( settings == null )
      return;
  }

  private URI findDocumentBase( final Shell shell, final String commandName, final IPdbConnection connection )
  {
    final PdbInfo info = connection.getInfo();

    try
    {
      return info.getDocumentBase();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      final String STR_ATTACHMENTS_DISABLED = Messages.getString( "CheckoutPdbData.3" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, STR_ATTACHMENTS_DISABLED, e );

      StatusDialog.open( shell, status, commandName );

      return null;
    }
  }

  public CheckoutDataMapping getMapping( )
  {
    return m_mapping;
  }

  public Feature[] getNewWspmElements( )
  {
    return m_mapping.getNewElements();
  }

  public boolean getConfirmExisting( )
  {
    return m_confirmExisting;
  }

  public void setConfirmExisting( final boolean confirmExisting )
  {
    final Object oldValue = m_confirmExisting;

    m_confirmExisting = confirmExisting;

    firePropertyChange( PROPERTY_CONFIRM_EXISTING, oldValue, confirmExisting );
  }

  public URI getDocumentBase( )
  {
    return m_documentBase;
  }

  public RemoveStrategy getRemoveStrategy( )
  {
    return m_removeStrategy;
  }

  public void setRemoveStrategy( final RemoveStrategy removeStrategy )
  {
    final RemoveStrategy oldValue = m_removeStrategy;

    m_removeStrategy = removeStrategy;

    firePropertyChange( PROPERTY_REMOVE_STRATEGY, oldValue, removeStrategy );
  }

  public IStatus initMapping( final Object[] elements, final CommandableWorkspace workspace, final TuhhWspmProject project )
  {
    try
    {
      final Object[] elementsUpToDate = refetchElements( m_session, elements );

      final CheckoutDataSearcher searcher = new CheckoutDataSearcher();
      searcher.search( elementsUpToDate );

      final WaterBody[] waterBodies = searcher.getWaterBodies();
      final State[] states = searcher.getStates();
      final CrossSection[] crossSections = searcher.getCrossSections();
      final Event[] events = searcher.getEvents();

      m_mapping = new CheckoutDataMapping( waterBodies, states, crossSections, events, workspace, project );

      return Status.OK_STATUS;
    }
    catch( final HibernateException e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
  }

  public void closeConnection( )
  {
    if( m_session != null )
    {
      PdbUtils.closeSessionQuietly( m_session );
      m_session = null;
    }

    if( m_connection != null )
    {
      PdbUtils.closeQuietly( m_connection );
      m_connection = null;
    }
  }

  public ICoefficients getCoefficients( )
  {
    return m_coefficients;
  }

  public GafCodes getCodes( )
  {
    return m_codes;
  }

  public int getDbSRID( )
  {
    return m_dbSRID;
  }
}