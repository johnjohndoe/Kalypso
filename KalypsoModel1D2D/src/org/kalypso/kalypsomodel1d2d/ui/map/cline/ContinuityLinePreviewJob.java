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
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine2DGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * Recalculates the continuity line for the preview.
 * 
 * @author Gernot Belger
 */
class ContinuityLinePreviewJob extends Job
{
  private GM_Curve m_contiLine;

  private final IFE1D2DNode[] m_nodes;

  public ContinuityLinePreviewJob( final IFE1D2DNode[] nodes )
  {
    super( "Calculate continuity line preview" ); //$NON-NLS-1$

    m_nodes = nodes;
  }

  public GM_Curve getContinuityLine( )
  {
    return m_contiLine;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      m_contiLine = calculateGeometry( monitor );

      return Status.OK_STATUS;
    }
    catch( final OperationCanceledException e )
    {
      return Status.CANCEL_STATUS;
    }
  }

  private GM_Curve calculateGeometry( final IProgressMonitor monitor )
  {
    if( m_nodes.length == 0 )
      return null;

    /* 1D conti-line */
    if( m_nodes.length == 1 && !ContinuityLineEditValidator.is2dNode( m_nodes[0] ) )
      return ContinuityLine1D.calculateGeometry( m_nodes[0] );

    /* 2D conti-line */
    final ContinuityLine2DGeometryBuilder builder = new ContinuityLine2DGeometryBuilder( m_nodes, monitor );
    return builder.getCurve();
  }
}