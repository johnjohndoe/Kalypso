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
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.ByStationComparator;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * @author Gernot Belger
 */
public class CheckoutOperation implements ICoreRunnableWithProgress
{
  private final IStructuredSelection m_selection;

  private final Set<CrossSection> m_crossSections = new HashSet<CrossSection>();

  private final IPdbWspmProject m_project;

  private TuhhReach[] m_changedReaches;

  public CheckoutOperation( final IPdbWspmProject project, final IStructuredSelection selection )
  {
    m_project = project;
    m_selection = selection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Loading data from cross section database", 100 );

    // TODO: save dirty project data; ask user to do so..

    monitor.subTask( "Searching for cross sections to checkout..." );
    findCrossSections();
    ProgressUtilities.worked( monitor, 5 );
    if( m_crossSections.isEmpty() )
      return new Status( IStatus.WARNING, WspmPdbCorePlugin.PLUGIN_ID, "No cross sections found in selection." );

    // TODO Preview?

    checkoutCrossSections( new SubProgressMonitor( monitor, 95 ) );

    ProgressUtilities.done( monitor );

    return Status.OK_STATUS;
  }

  private void findCrossSections( )
  {
    final List< ? > list = m_selection.toList();
    addElementsAsCrossSection( list );
  }

  private void addElementsAsCrossSection( final Collection< ? > elements )
  {
    for( final Object element : elements )
      addElementAsCrossSection( element );
  }

  private void addElementAsCrossSection( final Object element )
  {
    if( element instanceof CrossSection )
      m_crossSections.add( (CrossSection) element );
    else if( element instanceof State )
    {
      final State state = (State) element;
      addElementsAsCrossSection( state.getCrossSections() );
    }
    else if( element instanceof WaterBody )
    {
      final WaterBody waterBody = (WaterBody) element;
      addElementsAsCrossSection( waterBody.getCrossSections() );
    }
  }

  private void checkoutCrossSections( final IProgressMonitor monitor ) throws CoreException
  {
    final List<CrossSection> sortedSections = getSortedSections();
    monitor.beginTask( "Reading cross sections from database", sortedSections.size() + 10 );

    try
    {
      /* Convert the cross sections */
      final TuhhWspmProject project = m_project.getWspmProject();
      final CrossSectionInserter inserter = new CrossSectionInserter( project );
      for( final CrossSection crossSection : sortedSections )
      {
        monitor.subTask( String.format( "Converting %s", crossSection.getStation() ) );
        inserter.insert( crossSection );
        ProgressUtilities.worked( monitor, 1 );
      }

      m_changedReaches = inserter.getInsertedReches();
      final WspmWaterBody[] changedWaterBodies = getChangedParents( m_changedReaches );

      final CommandableWorkspace workspace = m_project.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, changedWaterBodies, m_changedReaches, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      workspace.postCommand( new EmptyCommand( null, false ) );
      m_project.doSave( new SubProgressMonitor( monitor, 10 ) );
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
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

  private WspmWaterBody[] getChangedParents( final TuhhReach[] changedReaches )
  {
    final Set<WspmWaterBody> result = new HashSet<WspmWaterBody>();

    for( final TuhhReach reach : changedReaches )
      result.add( reach.getWaterBody() );

    return result.toArray( new WspmWaterBody[result.size()] );
  }

  private List<CrossSection> getSortedSections( )
  {
    final ArrayList<CrossSection> list = new ArrayList<CrossSection>( m_crossSections );
    Collections.sort( list, new ByStationComparator() );
    return list;
  }

  public TuhhReach[] getNewReaches( )
  {
    return m_changedReaches;
  }
}