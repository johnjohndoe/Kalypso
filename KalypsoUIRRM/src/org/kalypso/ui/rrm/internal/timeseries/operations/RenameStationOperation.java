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

import java.net.URL;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenario;

/**
 * @author Dirk Kuch
 */
public class RenameStationOperation implements ICoreRunnableWithProgress
{
  private final IStation m_station;

  private final String m_oldFolder;

  private final String m_newFolder;

  private final URL[] m_oldTimeserieses;

  public RenameStationOperation( final IStation station, final String oldFolder, final String newFolder, final URL[] oldTimeserieses )
  {
    m_station = station;
    m_oldFolder = oldFolder;
    m_newFolder = newFolder;
    m_oldTimeserieses = oldTimeserieses;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    try
    {
      final IStatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

      final URL context = m_station.getWorkspace().getContext();

      final URL urlSource = UrlResolverSingleton.resolveUrl( context, m_oldFolder );
      final URL urlTarget = UrlResolverSingleton.resolveUrl( context, m_newFolder );

      final IFolder source = ResourceUtilities.findFolderFromURL( urlSource );
      final IFolder target = ResourceUtilities.findFolderFromURL( urlTarget );

      if( source.exists() )
        source.move( target.getFullPath(), true, new NullProgressMonitor() );

      target.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );

      final IFeatureBindingCollection<ITimeseries> timeserieses = m_station.getTimeseries();
      for( int index = 0; index < m_oldTimeserieses.length; index++ )
      {
        final URL oldTimeseries = m_oldTimeserieses[index];
        final ITimeseries timeseries = timeserieses.get( index );
        final ZmlLink link = timeseries.getDataLink();

        // FIXME: Handle all timeseries simultaniously... No outer loop...
        final IStatus updateStatus = doUpdateTimeseriesLinks( oldTimeseries, link.getHref() );
        stati.add( updateStatus );
      }

      return stati.asMultiStatusOrOK( Messages.getString( "RenameStationOperation_0" ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "RenameStationOperation_1" ), e ); //$NON-NLS-1$
    }
  }

  private IStatus doUpdateTimeseriesLinks( final URL oldTimeseries, final String href )
  {
    final IScenario scenario = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext().getCurrentCase();
    final TimeseriesReferencesUpdater updater = new TimeseriesReferencesUpdater( scenario, oldTimeseries, href );
    return updater.execute( new NullProgressMonitor() );
  }
}