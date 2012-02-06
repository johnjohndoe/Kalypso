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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.util.Date;
import java.util.Set;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;

/**
 * @author Gernot Belger
 */
public class SaveEventOperation implements IPdbOperation
{
  private final Event m_event;

  private final String m_username;

  public SaveEventOperation( final Event event, final String username )
  {
    m_event = event;
    m_username = username;
  }

  @Override
  public String getLabel( )
  {
    return "Saveevent";
  }

  @Override
  public void execute( final Session session )
  {
    /* Prepare event for save */
    final Date now = new Date();
    m_event.setCreationDate( now );
    m_event.setEditingDate( now );
    m_event.setEditingUser( m_username );

    session.save( m_event );

    final Set<WaterlevelFixation> waterlevels = m_event.getWaterlevelFixations();
    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      waterlevel.setCreationDate( now );
      waterlevel.setEditingDate( now );
      waterlevel.setEditingUser( m_username );
      waterlevel.setEvent( m_event );

      if( waterlevel.getMeasurementDate() == null )
        waterlevel.setMeasurementDate( m_event.getMeasurementDate() );

      session.save( waterlevel );
    }
  }
}