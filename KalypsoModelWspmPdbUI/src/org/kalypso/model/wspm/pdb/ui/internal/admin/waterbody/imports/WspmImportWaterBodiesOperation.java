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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports.ImportWaterBodiesData.INSERTION_MODE;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.SaveWaterBodyHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * The operation imports water bodies from a shape into a wspm project.
 * 
 * @author Gernot Belger
 */
public class WspmImportWaterBodiesOperation implements ICoreRunnableWithProgress
{
  /**
   * The workspace.
   */
  private final CommandableWorkspace m_workspace;

  /**
   * The water bodies.
   */
  private final WaterBody[] m_waterBodies;

  /**
   * The import water bodies data.
   */
  private final ImportWaterBodiesData m_data;

  /**
   * The wspm project.
   */
  private final WspmProject m_wspmProject;

  /**
   * The existing water bodies hashed by their gkz.
   */
  private final Map<String, WspmWaterBody> m_existingWaterBodies;

  /**
   * The added features.
   */
  private final List<Feature> m_added;

  /**
   * The changed features.
   */
  private final List<Feature> m_changed;

  /**
   * The constructor.
   * 
   * @param workspace
   *          The workspace.
   * @param waterBodies
   *          The water bodies.
   * @param data
   *          The import water bodies data.
   * @param wspmProject
   *          The wspm project.
   * @param existingWaterBodies
   *          The existing water bodies hashed by their gkz.
   */
  public WspmImportWaterBodiesOperation( final CommandableWorkspace workspace, final WaterBody[] waterBodies, final ImportWaterBodiesData data, final WspmProject wspmProject, final Map<String, WspmWaterBody> existingWaterBodies )
  {
    m_workspace = workspace;
    m_waterBodies = waterBodies;
    m_data = data;
    m_wspmProject = wspmProject;
    m_existingWaterBodies = existingWaterBodies;
    m_added = new ArrayList<>();
    m_changed = new ArrayList<>();
  }

  @Override
  public IStatus execute( IProgressMonitor monitor ) throws InvocationTargetException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* Monitor. */
    monitor.beginTask( Messages.getString( "WspmImportWaterBodiesOperation.0" ), 10 + 10 * m_waterBodies.length ); //$NON-NLS-1$

    try
    {
      /* Get the insertion mode. */
      final INSERTION_MODE insertionMode = m_data.getInsertionMode();

      /* Handle each water body. */
      for( final WaterBody waterBody : m_waterBodies )
      {
        /* Monitor. */
        monitor.subTask( waterBody.getName() );

        /* Insert/Update the water body. */
        insertWaterBody( insertionMode, waterBody );

        /* Monitor. */
        monitor.worked( 10 );
      }

      /* Monitor. */
      monitor.subTask( Messages.getString( "WspmImportWaterBodiesOperation.1" ) ); //$NON-NLS-1$

      /* Fire events. */
      fireEvents();

      /* Monitor. */
      monitor.worked( 10 );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void insertWaterBody( final INSERTION_MODE insertionMode, final WaterBody waterBody ) throws Exception
  {
    final WspmWaterBody existingWaterBody = m_existingWaterBodies.get( waterBody.getName() );
    if( existingWaterBody != null )
    {
      updateWaterBody( insertionMode, waterBody, existingWaterBody );
      return;
    }

    insert( waterBody );
  }

  private void updateWaterBody( final INSERTION_MODE insertionMode, final WaterBody waterBody, final WspmWaterBody existingWaterBody ) throws Exception
  {
    switch( insertionMode )
    {
      case skip:
        return;

      case overwrite:
        update( waterBody, existingWaterBody );
        return;
    }
  }

  private void insert( final WaterBody waterBody ) throws Exception
  {
    final SaveWaterBodyHelper helper = new SaveWaterBodyHelper( m_wspmProject, null );
    final WspmWaterBody feature = helper.updateOrCreateWspmWaterBody( waterBody, null );
    m_added.add( feature );
  }

  private void update( final WaterBody waterBody, final WspmWaterBody existingWaterBody ) throws Exception
  {
    final SaveWaterBodyHelper helper = new SaveWaterBodyHelper( m_wspmProject, null );
    final WspmWaterBody feature = helper.updateOrCreateWspmWaterBody( waterBody, existingWaterBody );
    m_changed.add( feature );
  }

  private void fireEvents( ) throws Exception
  {
    /* No workspace? */
    if( m_workspace == null )
      return;

    /* Convert to arrays. */
    final Feature[] added = m_added.toArray( new Feature[] {} );
    final Feature[] changed = m_changed.toArray( new Feature[] {} );

    /* Get the parents. */
    final Feature[] addedParents = findParents( added );
    for( final Feature changedParent : addedParents )
    {
      /* Get the children. */
      final Feature[] children = findChildren( changedParent, added );

      /* Fire the event. */
      m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, changedParent, children, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    }

    /* Fire the event. */
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, changed ) );

    /* Post the command. */
    m_workspace.postCommand( new EmptyCommand( null, false ) );
  }

  private Feature[] findParents( final Feature[] features )
  {
    final Collection<Feature> parents = new ArrayList<>();

    for( final Feature feature : features )
    {
      final Feature parent = feature.getOwner();
      if( parent != null )
        parents.add( parent );
    }

    return parents.toArray( new Feature[parents.size()] );
  }

  private Feature[] findChildren( final Feature parent, final Feature[] removedFeatures )
  {
    final Collection<Feature> children = new ArrayList<>();

    for( final Feature feature : removedFeatures )
    {
      final Feature owner = feature.getOwner();
      if( owner == parent )
        children.add( feature );
    }

    return children.toArray( new Feature[children.size()] );
  }
}