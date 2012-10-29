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
package org.kalypso.model.wspm.pdb.ui.internal.admin.event;

import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.wizard.Wizard;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.EventUtils;
import org.kalypso.model.wspm.pdb.db.utils.StateUtils;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IEditWorker;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.DefaultEditEventPageData;
import org.kalypso.model.wspm.pdb.wspm.ExistingEventsFetcher;
import org.kalypso.model.wspm.pdb.wspm.IEditEventPageData;

/**
 * @author Gernot Belger
 */
public class EditEventWorker implements IEditWorker
{
  private final Event m_selectedItem;

  private String m_nameToSelect;

  Event m_clone;

  private Event m_eventToEdit;

  private final String m_username;

  private final IPdbConnection m_connection;

  public EditEventWorker( final IPdbConnection connection, final Event selectedItem, final String username )
  {
    m_connection = connection;
    m_selectedItem = selectedItem;
    m_username = username;
    m_nameToSelect = m_selectedItem.getName();
  }

  @Override
  public String getWindowTitle( )
  {
    return Messages.getString( "EditEventWorker_0" ); //$NON-NLS-1$
  }

  @Override
  public Wizard createWizard( final IProgressMonitor monitor, final Session session )
  {
    monitor.subTask( Messages.getString( "EditEventWorker_1" ) ); //$NON-NLS-1$

    m_eventToEdit = EventUtils.findEventByName( session, m_selectedItem.getName() );

    final Collection<State> states = WaterBodyUtils.getPossibleStates( m_eventToEdit.getWaterBody() );

    m_clone = cloneForEdit( m_selectedItem, states );

    final ExistingEventsFetcher eventsFetcher = new ExistingEventsFetcher( m_connection, m_clone );

    final IEditEventPageData data = new DefaultEditEventPageData( eventsFetcher, m_clone, states, states != null );

    return new EditEventWizard( data );
  }

  @Override
  public void afterWizardOK( )
  {
    m_nameToSelect = m_clone.getName();
    uncloneData();
  }

  @Override
  public void addElementsToSelect( final ElementSelector selector )
  {
    selector.addEventName( m_nameToSelect );
  }

  private Event cloneForEdit( final Event other, final Collection<State> states )
  {
    final Set<WaterlevelFixation> waterLevelFixations = new HashSet<>( other.getWaterlevelFixations() );
    final Set<CrossSectionPart> crossSectionParts = new HashSet<>( other.getCrossSectionParts() );

    // TRICKY: we need to set the clone one of the states got from the water body, not its own state reference.
    // Thanks to hibernate's lazy loading, these might not be the same object instances, which would give problems later on.
    final State state = other.getState();
    final String stateName = state == null ? null : state.getName();
    final State foundState = StateUtils.findStateByName( states, stateName );

    return new Event( other.getId(), other.getWaterBody(), other.getName(), other.getCreationDate(), other.getEditingDate(), other.getEditingUser(), other.getMeasurementDate(), other.getSource(), other.getType(), other.getWlType(), other.getDescription(), waterLevelFixations, foundState, other.getStyleArray(), crossSectionParts );
  }

  /**
   * Copy the edited data back into the persistent object.
   */
  private void uncloneData( )
  {
    m_eventToEdit.setCreationDate( m_clone.getCreationDate() );
    m_eventToEdit.setDescription( m_clone.getDescription() );
    m_eventToEdit.setMeasurementDate( m_clone.getMeasurementDate() );
    m_eventToEdit.setName( m_clone.getName() );
    m_eventToEdit.setSource( m_clone.getSource() );
    m_eventToEdit.setType( m_clone.getType() );
    m_eventToEdit.setWaterBody( m_clone.getWaterBody() );
    m_eventToEdit.setState( m_clone.getState() );
    m_eventToEdit.setCrossSectionParts( m_clone.getCrossSectionParts() );
    m_eventToEdit.setStyleArray( m_clone.getStyleArray() );

    // Automatically change editing data and user
    m_eventToEdit.setEditingDate( new Date() );
    m_eventToEdit.setEditingUser( m_username );
  }
}