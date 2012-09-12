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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.ImportQuadMeshWorker;
import org.kalypso.kalypsomodel1d2d.ui.map.quadmesh.QuadMesh;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
final class ChannelEditApplyOperation implements ICoreRunnableWithProgress
{
  private final ChannelEditData m_data;

  public ChannelEditApplyOperation( final ChannelEditData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    m_data.setDelegate( null );

    final QuadMesh[] meshes = getMeshes();

    final CommandableWorkspace workspace = m_data.getDiscretisationWorkspace();

    final ImportQuadMeshWorker worker = new ImportQuadMeshWorker( workspace, meshes );
    return worker.execute( monitor );
  }

  /**
   * converts a Coordinate[][] array into a GM_Point[][] array
   */
  private QuadMesh[] getMeshes( )
  {
    final ISegmentData[] segments = m_data.getSegments();

    final Collection<QuadMesh> meshes = new ArrayList<>( segments.length );

    for( final ISegmentData segment : segments )
    {
      final QuadMesh mesh = segment.getMesh();
      if( mesh != null )
        meshes.add( mesh );
    }

    return meshes.toArray( new QuadMesh[meshes.size()] );
  }
}