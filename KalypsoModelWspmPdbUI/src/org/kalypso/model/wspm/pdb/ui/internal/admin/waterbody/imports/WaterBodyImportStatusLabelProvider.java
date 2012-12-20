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

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class WaterBodyImportStatusLabelProvider extends ColumnLabelProvider
{
  private final Set<String> m_names = new HashSet<>();

  private final Map<WaterBody, IStatus> m_waterBodyStatus;

  public WaterBodyImportStatusLabelProvider( final WaterBody[] existingWaterBodies, final Map<WaterBody, IStatus> waterBodyStatus )
  {
    m_waterBodyStatus = waterBodyStatus;

    if( existingWaterBodies != null )
    {
      for( final WaterBody waterBody : existingWaterBodies )
        m_names.add( waterBody.getName() );
    }
  }

  @Override
  public String getText( final Object element )
  {
    final IStatus status = getStatus( element );
    return status.getMessage();
  }

  @Override
  public Image getImage( final Object element )
  {
    final IStatus status = getStatus( element );
    return StatusComposite.getStatusImage( status );
  }

  private IStatus getStatus( final Object element )
  {
    if( element instanceof WaterBody )
    {
      final WaterBody water = (WaterBody) element;

      final IStatusCollector combinedStatus = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

      final String name = water.getName();
      if( m_names.contains( name ) )
        combinedStatus.add( IStatus.INFO, Messages.getString( "WaterBodyImportStatusLabelProvider.0" ) ); //$NON-NLS-1$

      if( m_waterBodyStatus.containsKey( water ) )
        combinedStatus.add( m_waterBodyStatus.get( water ) );

      if( combinedStatus.isEmpty() )
        return Status.OK_STATUS;

      if( combinedStatus.size() == 1 )
        return combinedStatus.getAllStati()[0];

      return combinedStatus.asMultiStatusOrOK( Messages.getString( "WaterBodyImportStatusLabelProvider.1" ) ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }
}