/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.ogc.gml.map.IMapPanel;

/**
 * Helper for building the complete continuity line.
 * 
 * @author Gernot Belger
 */
class ContinuityLineBuilder
{
  private final List<IFE1D2DNode> m_nodes = new ArrayList<>();

  public void addPath( final IFE1D2DNode[] path )
  {
    if( path == null )
      return;

    for( final IFE1D2DNode node : path )
      addNode( node );
  }

  private void addNode( final IFE1D2DNode node )
  {
    final IFE1D2DNode lastNode = m_nodes.isEmpty() ? null : m_nodes.get( m_nodes.size() - 1 );
    if( node != lastNode )
      m_nodes.add( node );
  }

  public IFE1D2DNode[] getContinuityLine( )
  {
    return m_nodes.toArray( new IFE1D2DNode[m_nodes.size()] );
  }

  public IStatus validate( final IMapPanel panel )
  {
    final IFE1D2DNode[] continuityLine = getContinuityLine();

    final ContinuityLine2DValidator validator = new ContinuityLine2DValidator( panel, continuityLine );

    final String message = validator.execute();
    if( message == null )
      return Status.OK_STATUS;

    return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, message );
  }
}
