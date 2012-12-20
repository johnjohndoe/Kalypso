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
package org.kalypso.kalypsomodel1d2d.ui.map.quadmesh;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.Add2DElementsCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;

/**
 * Imports a QuadMesh into a discretisation model.
 * 
 * @author Gernot Belger
 */
public class ImportQuadMeshWorker implements ICoreRunnableWithProgress
{
  private final CommandableWorkspace m_discretisationWorkspace;

  private final QuadMesh[] m_grids;

  public ImportQuadMeshWorker( final CommandableWorkspace discretisationWorkspace, final QuadMesh... grids )
  {
    m_discretisationWorkspace = discretisationWorkspace;
    m_grids = grids;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    try
    {
      /* Fetch all rings */
      final List<GM_PolygonPatch> rings = new ArrayList<>();
      for( final QuadMesh mesh : m_grids )
      {
        final List<GM_PolygonPatch> meshRings = mesh.toRings();
        rings.addAll( meshRings );
      }

      /* Add rings as 2d elements to net */
      final Add2DElementsCommand command = new Add2DElementsCommand( m_discretisationWorkspace, rings, monitor );
      m_discretisationWorkspace.postCommand( command );
      return command.getStatus();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.2" ), e ); //$NON-NLS-1$
    }
  }
}