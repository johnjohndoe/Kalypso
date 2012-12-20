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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;

/**
 * @author Gernot Belger
 */
public class RoughnessHandler
{
  private final IStatusCollector m_stati = new StatusCollector( KalypsoModel1D2DPlugin.PLUGIN_ID );

  private final Map<String, String> m_protoClasses = new HashMap<>();

  private final Map<String, IRoughnessCls> m_newClasses = new HashMap<>();

  private final IRoughnessClsCollection m_roughnessModel;

  public RoughnessHandler( final IRoughnessClsCollection roughnessModel )
  {
    m_roughnessModel = roughnessModel;
  }

  public void addRoughness( final String id, final String label )
  {
    if( m_protoClasses.containsKey( id ) )
      m_stati.add( IStatus.WARNING, Messages.getString("RoughnessHandler_0"), null, id ); //$NON-NLS-1$
    else
      m_protoClasses.put( id, label );
  }

  /**
   * @return <code>true</code>, is something changed.
   */
  public boolean changeModel( )
  {
    insertClasses();

    /* Fire feature events */
    fireEvents( m_roughnessModel, m_newClasses.values() );

    return m_newClasses.size() > 0;
  }

  private void insertClasses( )
  {
    final IdMap existingClasses = buildIdMap();

    for( final Entry<String, String> entry : m_protoClasses.entrySet() )
    {
      final String id = entry.getKey();
      final String label = entry.getValue();
      insertClass( id, label, existingClasses );
    }
  }

  private static void fireEvents( final Feature parentFeature, final Collection< ? extends Feature> changed )
  {
    final Feature[] changedFeatures = changed.toArray( new Feature[changed.size()] );
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    final ModellEvent event = new FeatureStructureChangeModellEvent( workspace, parentFeature, changedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
    workspace.fireModellEvent( event );
  }

  private IdMap buildIdMap( )
  {
    final IdMap roughnessIndex = new IdMap( m_roughnessModel.getRoughnessClasses().size() );
    for( final IRoughnessCls o : m_roughnessModel.getRoughnessClasses() )
    {
      roughnessIndex.getOrAdd( o.getName() );
    }

    return roughnessIndex;
  }

  private void insertClass( final String id, final String label, final IdMap existingClasses )
  {
    if( existingClasses.contains( id ) )
    {
      m_stati.add( IStatus.INFO, Messages.getString("RoughnessHandler_1"), null, id ); //$NON-NLS-1$
      return;
    }

    final IFeatureBindingCollection<IRoughnessCls> classes = m_roughnessModel.getRoughnessClasses();

    final IRoughnessCls newClass = classes.addNew( IRoughnessCls.QNAME, IRoughnessCls.class );
    newClass.setName( id );
    newClass.setDescription( label );

    m_newClasses.put( id, newClass );
  }

  public IStatus getStatus( )
  {
    return m_stati.asMultiStatusOrOK( Messages.getString("RoughnessHandler_2") ); //$NON-NLS-1$
  }
}