/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.command;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author belger
 */
public class ChangeFeaturesCommand implements ICommand
{
  private final FeatureChange[] m_newChanges;

  private final FeatureChange[] m_oldChanges;

  private final GMLWorkspace m_workspace;

  public ChangeFeaturesCommand( final GMLWorkspace workspace, final FeatureChange[] changes )
  {
    m_workspace = workspace;
    m_newChanges = changes;
    m_oldChanges = new FeatureChange[changes.length];
    for( int i = 0; i < changes.length; i++ )
    {
      final FeatureChange change = changes[i];

      final Object oldValue = change.getFeature().getProperty( change.getProperty() );
      m_oldChanges[i] = new FeatureChange( change.getFeature(), change.getProperty(), oldValue );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process() throws Exception
  {
    applyChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    applyChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    applyChanges( m_oldChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature ver�ndern";
  }

  private void applyChanges( final FeatureChange[] changes )
  {
    final List<Feature> changedFeaturesList = new ArrayList<Feature>();
    for( int i = 0; i < changes.length; i++ )
    {
      final FeatureChange change = changes[i];
      final FeatureProperty fp = FeatureFactory.createFeatureProperty( change.getProperty(), change.getNewValue() );
      change.getFeature().setProperty( fp );
      changedFeaturesList.add( change.getFeature() );
    }

    if( m_workspace != null )
      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, changedFeaturesList ) );
  }
}
