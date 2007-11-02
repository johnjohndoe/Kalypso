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

import java.util.HashSet;
import java.util.Set;

import org.kalypso.commons.command.ICommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class ChangeFeaturesCommand implements ICommand
{
  private final FeatureChange[] m_newChanges;

  private final FeatureChange[] m_oldChanges;

  protected final GMLWorkspace m_workspace;

  public ChangeFeaturesCommand( final GMLWorkspace workspace, final FeatureChange[] changes )
  {
    // just to avoid the exception...
    if( changes == null )
      m_newChanges = new FeatureChange[0];
    else
      m_newChanges = changes;
    m_workspace = workspace;
    m_oldChanges = new FeatureChange[m_newChanges.length];
    for( int i = 0; i < m_newChanges.length; i++ )
    {
      final FeatureChange change = m_newChanges[i];

      final Object oldValue = change.getFeature().getProperty( change.getProperty() );
      m_oldChanges[i] = new FeatureChange( change.getFeature(), change.getProperty(), oldValue );
// System.out.println("FC prop: " + change.getProperty());
    }
  }

  public FeatureChange[] getFeatureChanges( )
  {
    return m_newChanges;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    applyChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    applyChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    applyChanges( m_oldChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Feature verändern";
  }

  protected void applyChanges( final FeatureChange[] changes )
  {
    final Set<Feature> changedFeaturesList = new HashSet<Feature>();
    for( final FeatureChange change : changes )
    {
      change.getFeature().setProperty( change.getProperty(), change.getNewValue() );
      changedFeaturesList.add( change.getFeature() );
    }

    if( m_workspace != null )
      m_workspace.fireModellEvent( new FeatureChangeModellEvent( m_workspace, changes ) );
  }
}
