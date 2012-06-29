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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.project.RrmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dirk Kuch
 */
public final class UpdateTimeseriesLinks
{
  private UpdateTimeseriesLinks( )
  {
  }

  public static IStatus doUpdateTimeseriesLinks( final IProject project, final URL old, final String href )
  {
    final UpdateTimeseriesLinksVisitor visitor = new UpdateTimeseriesLinksVisitor( old, href );

    final RrmProject rrmProject = new RrmProject( project );
    final IFolder baseFolder = rrmProject.getBaseFolder();

    return doUpdateTimeseriesLinks( baseFolder, visitor );

  }

  public static IStatus doUpdateTimeseriesLinks( final ITimeseries timeseries, final ITimeseries current )
  {
    final UpdateTimeseriesLinksVisitor visitor = new UpdateTimeseriesLinksVisitor( timeseries, current );

    final RrmProject rrmProject = new RrmProject( timeseries.getDataLink().getFile().getProject() );
    final IFolder baseFolder = rrmProject.getBaseFolder();

    return doUpdateTimeseriesLinks( baseFolder, visitor );
  }

  private static IStatus doUpdateTimeseriesLinks( final IFolder modelFolder, final UpdateTimeseriesLinksVisitor visitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    stati.add( doUpdateTimeseriesLinks( modelFolder.getFile( ".models/modell.gml" ), visitor ) ); //$NON-NLS-1$
    stati.add( doUpdateTimeseriesLinks( modelFolder.getFile( ".models/catchmentModels.gml" ), visitor ) ); //$NON-NLS-1$

    return stati.asMultiStatusOrOK( Messages.getString( "UpdateTimeseriesLinks_0" ) ); //$NON-NLS-1$
  }

  private static IStatus doUpdateTimeseriesLinks( final IFile file, final UpdateTimeseriesLinksVisitor visitor )
  {
    try
    {
      final GMLWorkspaceChangedListener listener = new GMLWorkspaceChangedListener();

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( file );
      workspace.addModellListener( listener );
      workspace.accept( visitor, FeatureVisitor.DEPTH_INFINITE );

      if( listener.isWorkspaceChanged() )
        GmlSerializer.saveWorkspace( workspace, file );
    }
    catch( final Exception e )
    {
      final String msg = String.format( Messages.getString( "UpdateTimeseriesLinks_1" ), file.getFullPath().toOSString() ); //$NON-NLS-1$
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), msg, e );
    }

    final String msg = String.format( Messages.getString( "UpdateTimeseriesLinks_2" ), file.getFullPath().toOSString() ); //$NON-NLS-1$

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), msg );
  }
}
