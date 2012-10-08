/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.hibernate.Session;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.utils.StateUtils;
import org.kalypso.model.wspm.pdb.wspm.IEditEventPageData;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;

/**
 * @author Gernot Belger
 */
public class ImportWaterLevelsData extends AbstractModelObject implements IEditEventPageData
{
  private final Event m_event = new Event();

  private final IPdbConnection m_connection;

  private Event[] m_existingEvents;

  private String m_shapeFile;

  private String m_shapeSRS;

  private ImportAttributeInfo< ? >[] m_infos;

  private Collection<State> m_states = Collections.emptyList();

  public ImportWaterLevelsData( final IPdbConnection connection )
  {
    m_connection = connection;

    m_event.setMeasurementDate( new Date() );
  }

  public void init( final IDialogSettings dialogSettings ) throws PdbConnectException
  {
    Session session = null;
    try
    {
      session = m_connection.openSession();

      m_existingEvents = GetPdbList.getArray( session, Event.class );

      session.close();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }

    if( dialogSettings == null )
      return;
  }

  public void setShapeInput( final String shapeFile, final String srs )
  {
    if( shapeFile == null )
      m_shapeFile = null;
    else
    {
      if( shapeFile.toLowerCase().endsWith( ShapeFile.EXTENSION_SHP ) )
        m_shapeFile = FilenameUtils.removeExtension( shapeFile );
      else
        m_shapeFile = shapeFile;
    }

    m_shapeSRS = srs;

    /* Update name and source of event */
    if( m_shapeFile != null )
    {
      final String baseName = FilenameUtils.getBaseName( m_shapeFile );
      if( StringUtils.isBlank( m_event.getName() ) )
        m_event.setName( WordUtils.capitalizeFully( baseName.replace( '_', ' ' ) ) );

      if( StringUtils.isBlank( m_event.getSource() ) )
        m_event.setSource( baseName + ShapeFile.EXTENSION_SHP );
    }
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  @Override
  public Event[] getExistingEvents( )
  {
    return m_existingEvents;
  }

  @Override
  public Event getEvent( )
  {
    return m_event;
  }

  public ShapeFile openShape( ) throws IOException, DBaseException
  {
    return new ShapeFile( m_shapeFile, Charset.defaultCharset(), FileMode.READ );
  }

  public ImportAttributeInfo< ? >[] getAttributeInfos( )
  {
    return m_infos;
  }

  public void setAttributeInfos( final ImportAttributeInfo< ? >[] infos )
  {
    m_infos = infos;
  }

  public String getSrs( )
  {
    return m_shapeSRS;
  }

  @Override
  public Collection<State> getStates( )
  {
    return m_states;
  }

  public void setStates( final Collection<State> states )
  {
    final Collection<State> oldValue = m_states;

    m_states = states;

    firePropertyChange( PROPERTY_STATES, oldValue, states );

    // TRICKY: need to preserve state by hand, because instances may have changed
    final State state = m_event.getState();
    final String stateName = state == null ? null : state.getName();
    final State foundState = StateUtils.findStateByName( states, stateName );
    m_event.setState( foundState );
  }

  @Override
  public boolean showStatesChooser( )
  {
    return true;
  }

  /**
   * @return The srid of the database we are connected to.
   */
  public int getDbSRID( )
  {
    return m_connection.getInfo().getSRID();
  }
}