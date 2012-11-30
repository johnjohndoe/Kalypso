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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.utils.log.GeoStatusLog;

/**
 * @author Dirk Kuch
 */
public class StoreTimeseriesStatusOperation implements ICoreRunnableWithProgress
{
  private final IStatus m_status;

  private final ITimeseries m_timeseries;

  public StoreTimeseriesStatusOperation( final ITimeseries timeseries, final IStatus status )
  {
    m_timeseries = timeseries;
    m_status = status;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_timeseries == null )
      return Status.OK_STATUS;

    final IFile zmlFile = m_timeseries.getDataLink().getFile();

    final IFolder folder = (IFolder) zmlFile.getParent();
    final IFile status = folder.getFile( zmlFile.getName() + ".status" ); //$NON-NLS-1$

    final GeoStatusLog log = new GeoStatusLog( status );
    log.log( m_status );
    log.serialize();

    status.refreshLocal( IResource.DEPTH_ONE, monitor );

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "StoreTimeseriesStatusOperation_1" ) ); //$NON-NLS-1$
  }
}