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
package org.kalypso.statistics.gui.wizards.importNodesFromSHP;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.model.hydrology.operation.hydrotope.AbstractImportOperation;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.types.ENodeProfileType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger, Dejan Antanaskovic
 */
public class NodeImportOperation extends AbstractImportOperation<GM_Point>
{
  public static interface InputDescriptor extends AbstractImportOperation.InputDescriptor<GM_Point>
  {
    String getNodeLabel( int index ) throws CoreException;
  }

  private final InputDescriptor m_inputDescriptor;

  private final ENodeProfileType m_profileType;

  /**
   * @param profileType
   * @param output
   *          An (empty) list containing rrmgeology:geology features
   */
  public NodeImportOperation( final InputDescriptor inputDescriptor, final ENodeProfileType profileType )
  {
    super( inputDescriptor );
    m_inputDescriptor = inputDescriptor;
    m_profileType = profileType == null ? ENodeProfileType.HYDROLOGICAL_NODE : profileType;
  }

  @Override
  protected void init( )
  {
  }

  @Override
  protected Feature importRow( final int i, final String label, final GM_Point geometry, final IStatusCollector log ) throws CoreException
  {
    final NodeProfile profile = new NodeProfile( 0, m_inputDescriptor.getNodeLabel( i ) );
    profile.setDescription( m_inputDescriptor.getDescription( i ) );
    profile.setNodeProfileType( m_profileType );
    SessionDataProvider.getInstance().getDataProvider().getDbHandlerNodeProfile().saveRecord( profile );

    return null;
  }
}
