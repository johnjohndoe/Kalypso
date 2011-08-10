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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbOperation implements ICoreRunnableWithProgress
{
  private final List<Feature> m_changedFeatures = new ArrayList<Feature>();

  private final List<Feature> m_changedParents = new ArrayList<Feature>();

  private final ICheckoutPdbData m_data;

  public CheckoutPdbOperation( final ICheckoutPdbData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Loading data from cross section database", 100 );

    // FIXME: overwrite existing data ->
    // First: create/update all water bodies
    // Second: delete all existing states
    // third: delete all existing water levels
    // last: download data as before

    final ICheckoutElements elements = m_data.getElements();
    final CrossSection[] crossSections = elements.getCrossSections();
    final Event[] events = elements.getEvents();

    final boolean hasCrossSection = !ArrayUtils.isEmpty( crossSections );
    final boolean hasWaterlevels = !ArrayUtils.isEmpty( events );
    if( !hasCrossSection && !hasWaterlevels )
      return new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, "No downloadable data found in selection." );

    final TuhhWspmProject project = m_data.getWspmProject();
    final CheckoutCrossSectionsWorker crossSectionsWorker = new CheckoutCrossSectionsWorker( this, project, crossSections );
    crossSectionsWorker.execute( new SubProgressMonitor( monitor, 45 ) );

    final CheckoutWaterlevelWorker waterlevelWorker = new CheckoutWaterlevelWorker( this, project, events );
    waterlevelWorker.execute( new SubProgressMonitor( monitor, 45 ) );

    final Feature[] newElements = fireEvents( new SubProgressMonitor( monitor, 5 ) );
    m_data.setNewWspmElements( newElements );

    ProgressUtilities.done( monitor );

    return Status.OK_STATUS;
  }

  private Feature[] fireEvents( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final Feature[] changedFeatures = m_changedFeatures.toArray( new Feature[m_changedFeatures.size()] );
      final Feature[] changedParents = m_changedParents.toArray( new Feature[m_changedParents.size()] );

      final CommandableWorkspace workspace = m_data.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, changedParents, changedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      workspace.postCommand( new EmptyCommand( null, false ) );

      return changedFeatures;
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  void addChangedFeatures( final Feature[] changedFeatures )
  {
    m_changedFeatures.addAll( Arrays.asList( changedFeatures ) );

    for( final Feature feature : changedFeatures )
    {
      final Feature parent = feature.getParent();
      if( parent != null )
        m_changedParents.add( parent );
    }
  }
}