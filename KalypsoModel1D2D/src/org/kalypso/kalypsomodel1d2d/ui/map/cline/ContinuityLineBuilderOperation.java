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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.ogc.gml.map.IMapPanel;

/**
 * @author Gernot Belger
 */
class ContinuityLineBuilderOperation implements ICoreRunnableWithProgress
{
  private final ContinuityEdge[] m_edges;

  private IFE1D2DNode[] m_continuityLine;

  private final IMapPanel m_panel;

  public ContinuityLineBuilderOperation( final IMapPanel panel, final ContinuityEdge[] edges )
  {
    m_panel = panel;
    m_edges = edges;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InterruptedException
  {
    final ContinuityLineBuilder builder = new ContinuityLineBuilder();

    for( final ContinuityEdge edge : m_edges )
    {
      final IFE1D2DNode[] path = edge.waitForPath();
      builder.addPath( path );
    }

    m_continuityLine = builder.getContinuityLine();

    return builder.validate( m_panel );
  }

  public IFE1D2DNode[] getContinuityLine( )
  {
    return m_continuityLine;
  }
}