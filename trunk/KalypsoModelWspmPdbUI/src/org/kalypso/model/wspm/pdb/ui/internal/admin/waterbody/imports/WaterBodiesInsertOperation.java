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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports.ImportWaterBodiesData.INSERTION_MODE;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WaterBodiesInsertOperation implements IPdbOperation
{
  private final WaterBody[] m_waterBodies;

  private final Map<String, WaterBody> m_existingWaterBodies = new HashMap<>();

  private final INSERTION_MODE m_insertionMode;

  private final IProgressMonitor m_monitor;

  public WaterBodiesInsertOperation( final WaterBody[] waterBodies, final INSERTION_MODE insertionMode, final IProgressMonitor monitor )
  {
    m_waterBodies = waterBodies;
    m_insertionMode = insertionMode;
    m_monitor = monitor;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "WaterBodiesInsertOperation.0" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    m_monitor.beginTask( Messages.getString( "WaterBodiesInsertOperation.1" ), m_waterBodies.length * 2 + 20 ); //$NON-NLS-1$

    m_monitor.subTask( Messages.getString( "WaterBodiesInsertOperation.2" ) ); //$NON-NLS-1$
    final List<WaterBody> existingWaterBodies = GetPdbList.getList( session, WaterBody.class );
    for( final WaterBody waterBody : existingWaterBodies )
      m_existingWaterBodies.put( waterBody.getName(), waterBody );
    m_monitor.worked( 20 );

    for( final WaterBody waterBody : m_waterBodies )
    {
      m_monitor.subTask( waterBody.getName() );
      insertWaterBody( session, waterBody );
      m_monitor.worked( 1 );
    }

    m_monitor.subTask( Messages.getString( "WaterBodiesInsertOperation.3" ) ); //$NON-NLS-1$
    session.flush();

    m_monitor.done();
  }

  private void insertWaterBody( final Session session, final WaterBody waterBody )
  {
    final String name = waterBody.getName();
    final WaterBody existingWaterBody = m_existingWaterBodies.get( name );
    if( existingWaterBody != null )
      updateWaterBody( waterBody, existingWaterBody );
    else
      session.save( waterBody );
  }

  private void updateWaterBody( final WaterBody waterBody, final WaterBody existingWaterBody )
  {
    switch( m_insertionMode )
    {
      case skip:
        return;

      case overwrite:
        existingWaterBody.setAll( waterBody );
        return;
    }
  }
}