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
package org.kalypso.ui.rrm.internal.calccase;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * @author Gernot Belger
 */
public class UpdateCalcCaseOperation extends WorkspaceModifyOperation
{
  private final IFolder[] m_calcCases;

  public UpdateCalcCaseOperation( final IFolder[] calcCases )
  {
    // Find least common container and use as scheduling rule
    m_calcCases = calcCases;
  }

  @Override
  protected void execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Updating calc case(s)", m_calcCases.length );

    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    for( final IFolder calcCaseFolder : m_calcCases )
    {
      try
      {
        final UpdateSimulationWorker worker = new UpdateSimulationWorker( calcCaseFolder );

        final IStatus status = worker.execute( new SubProgressMonitor( monitor, 1 ) );
        collector.add( status );
      }
      catch( final CoreException e )
      {
        collector.add( e.getStatus() );
      }

      /* Check for cancel */
      ProgressUtilities.worked( monitor, 0 );
    }

    final IStatus resultStatus = collector.asMultiStatusOrOK( "Problem(s) occured during update" );
    if( !resultStatus.isOK() )
      throw new CoreException( resultStatus );
  }
}