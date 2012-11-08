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
package org.kalypso.model.wspm.pdb.wspm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.EventByNameComparator;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.wspm.AddKeysWithMappingClosure;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * @author Gernot Belger
 */
public class CheckoutDataMapping
{
  private final List<Feature> m_changedFeatures = new ArrayList<>();

  private final List<Feature> m_addedFeatures = new ArrayList<>();

  private final List<Feature> m_removedFeatures = new ArrayList<>();

  private final Map<WaterBody, WspmWaterBody> m_waterMapping = new HashMap<>();

  private final Map<State, TuhhReach> m_stateMapping = new HashMap<>();

  private final Map<Event, Object> m_eventMapping = new HashMap<>();

  private final CrossSection[] m_crossSections;

  private final TuhhWspmProject m_project;

  private final CommandableWorkspace m_workspace;

  public CheckoutDataMapping( final WaterBody[] waterBodies, final State[] states, final CrossSection[] crossSections, final Event[] events, final CommandableWorkspace workspace, final TuhhWspmProject wspmProject )
  {
    m_crossSections = crossSections;
    m_workspace = workspace;
    m_project = wspmProject;

    /* Build the mappings */
    buildWaterMapping( waterBodies );
    buildStateMapping( states );
    buildEventMapping( events );
  }

  private void buildWaterMapping( final WaterBody[] waterBodies )
  {
    for( final WaterBody waterBody : waterBodies )
    {
      final String gkn = waterBody.getName();
      final WspmWaterBody wspmWaterBody = m_project.findWaterByRefNr( gkn );
      m_waterMapping.put( waterBody, wspmWaterBody );
    }
  }

  private void buildStateMapping( final State[] states )
  {
    for( final State state : states )
    {
      final TuhhReach reach = findReach( state );
      m_stateMapping.put( state, reach );
    }
  }

  private TuhhReach findReach( final State state )
  {
    final WaterBody waterBody = findWaterBody( state );
    final WspmWaterBody wspmWaterBody = m_waterMapping.get( waterBody );
    /* If the water body is not in wspm, the state cannot be there as well */
    if( wspmWaterBody == null )
      return null;

    final String stateName = state.getName();
    return (TuhhReach)wspmWaterBody.findReachByName( stateName );
  }

  private void buildEventMapping( final Event[] events )
  {
    for( final Event event : events )
    {
      final Object waterlevel = findWaterlevel( event );
      m_eventMapping.put( event, waterlevel );
    }
  }

  private Object findWaterlevel( final Event event )
  {
    final WaterBody waterBody = event.getWaterBody();
    final WspmWaterBody wspmWaterBody = m_waterMapping.get( waterBody );
    /* If the water body is not in wspm, the state cannot be there as well */
    if( wspmWaterBody == null )
      return null;

    final String eventName = event.getName();

    return wspmWaterBody.findFixationByName( eventName );

    // REMARK / FIX: waterlevels from pdb are always checked out as fixations, so we only need
    // to compare with those
//    final TYPE type = event.getType();
//    switch( type )
//    {
//      case Measurement:
//
//      case Simulation:
//        final String simulationName = String.format( "%s_%s", waterBody.getName(), eventName ); //$NON-NLS-1$
//        return m_project.findCalculationByName( simulationName );
//    }
//
//    throw new IllegalStateException();
  }

  public TuhhWspmProject getProject( )
  {
    return m_project;
  }

  public CrossSection[] getCrossSections( )
  {
    return m_crossSections;
  }

  public WaterBody[] getWaterBodies( )
  {
    return m_waterMapping.keySet().toArray( new WaterBody[m_waterMapping.size()] );
  }

  public WspmWaterBody getWspmWaterBody( final WaterBody waterBody )
  {
    return m_waterMapping.get( waterBody );
  }

  public void set( final WaterBody waterBody, final WspmWaterBody wspmWater )
  {
    if( !m_waterMapping.containsKey( waterBody ) )
      throw new IllegalArgumentException();

    m_waterMapping.put( waterBody, wspmWater );
  }

  public State[] getStates( )
  {
    return m_stateMapping.keySet().toArray( new State[m_stateMapping.size()] );
  }

  public TuhhReach getReach( final State state )
  {
    return m_stateMapping.get( state );
  }

  public void set( final State state, final TuhhReach reach )
  {
    if( !m_stateMapping.containsKey( state ) )
      throw new IllegalArgumentException();

    m_stateMapping.put( state, reach );
  }

  public WaterBody findWaterBody( final State state )
  {
    final Set<CrossSection> crossSections = state.getCrossSections();
    // TRICKY: we just take the water body of the first cross-section as wb of this state -> problems?
    if( crossSections.isEmpty() )
      return null;

    return crossSections.iterator().next().getWaterBody();
  }

  public Event[] getEvents( )
  {
    final Event[] unsortedEvents = m_eventMapping.keySet().toArray( new Event[m_eventMapping.size()] );

    // REMARK: sort for checkout, so they are nicely sorted in the wspm tree
    Arrays.sort( unsortedEvents, new EventByNameComparator() );

    return unsortedEvents;
  }

  public Object getWaterlevel( final Event event )
  {
    return m_eventMapping.get( event );
  }

  void fireEvents( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final Feature[] removedFeatures = m_removedFeatures.toArray( new Feature[m_removedFeatures.size()] );
      final Feature[] addedFeatures = m_addedFeatures.toArray( new Feature[m_addedFeatures.size()] );

      // REMARK: we do fire an event for each parent, else we get refresh problems
      final Feature[] removedParents = findParents( m_removedFeatures );
      for( final Feature removedParent : removedParents )
      {
        final Feature[] children = findChildren( removedParent, removedFeatures );
        m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, removedParent, children, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
      }

      final Feature[] addedParents = findParents( m_addedFeatures );
      for( final Feature changedParent : addedParents )
      {
        final Feature[] children = findChildren( changedParent, addedFeatures );
        m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, changedParent, children, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      final Feature[] changedFeatures = getChangedFeatures();
      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, changedFeatures ) );

      m_workspace.postCommand( new EmptyCommand( null, false ) );
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

  private Feature[] findParents( final List<Feature> features )
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

  public void addChangedFeatures( final Feature changedFeature )
  {
    m_changedFeatures.add( changedFeature );
  }

  public void addAddedFeatures( final Feature newFeature )
  {
    m_addedFeatures.add( newFeature );
  }

  public void addRemovedFeatures( final Feature feature )
  {
    m_removedFeatures.add( feature );
  }

  public Feature[] getNewElements( )
  {
    return m_addedFeatures.toArray( new Feature[m_addedFeatures.size()] );
  }

  public Set<Object> getAllPdbElements( )
  {
    final Set<Object> all = new HashSet<>();

    all.addAll( Arrays.asList( m_crossSections ) );
    all.addAll( m_eventMapping.keySet() );
    all.addAll( m_waterMapping.keySet() );
    all.addAll( m_stateMapping.keySet() );

    return Collections.unmodifiableSet( all );
  }

  /**
   * All pdb elements that have an associated wspm element.
   */
  public Set<Object> getAllPdbElementsWithWspm( )
  {
    final Set<Object> all = new HashSet<>();

    CollectionUtils.forAllDo( m_eventMapping.keySet(), new AddKeysWithMappingClosure( m_eventMapping, all ) );
    // REMAR: for the moment, we ignore rivers, as overwriting causes them now problem (because they are not editable).
    // CollectionUtils.forAllDo( m_waterMapping.keySet(), new AddKeysWithMappingClosure( m_waterMapping, all ) );
    CollectionUtils.forAllDo( m_stateMapping.keySet(), new AddKeysWithMappingClosure( m_stateMapping, all ) );

    return Collections.unmodifiableSet( all );
  }

  public void featureRemoved( final Feature element )
  {
    addRemovedFeatures( element );

    removeFromMapping( m_eventMapping, element );
    removeFromMapping( m_stateMapping, element );
    removeFromMapping( m_waterMapping, element );
  }

  private void removeFromMapping( final Map< ? , ? > mapping, final Feature element )
  {
    if( !mapping.containsValue( element ) )
      return;

    for( final Entry< ? , ? > entry : mapping.entrySet() )
    {
      if( element.equals( entry.getValue() ) )
        entry.setValue( null );
    }
  }

  public Feature[] getChangedFeatures( )
  {
    return m_changedFeatures.toArray( new Feature[m_changedFeatures.size()] );
  }
}