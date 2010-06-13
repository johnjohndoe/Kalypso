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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * 
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class ListPropertyChangeCommand implements ICommand
{
  private final FeatureChange[] m_newChanges;

  private final FeatureChange[] m_oldChanges;

  private final GMLWorkspace m_workspace;

  public ListPropertyChangeCommand( final GMLWorkspace workspace, final FeatureChange[] changes )
  {
    m_workspace = workspace;
    m_newChanges = changes;
    m_oldChanges = new FeatureChange[changes.length];
    // check changes
    for( int i = 0; i < changes.length; i++ )
    {
      final FeatureChange change = changes[i];
      if( !change.getProperty().isList() )
      {
        throw new IllegalArgumentException( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ListPropertyChangeCommand.0") ); //$NON-NLS-1$
      }
      // final Object oldValue =
      // change.getFeature().getProperty( change.getProperty() );

    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    doChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    doChanges( m_newChanges );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    undoChanges();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ListPropertyChangeCommand.1"); //$NON-NLS-1$
  }

  private void doChanges( final FeatureChange[] changes )
  {
    final Set<Feature> changedFeaturesList = new HashSet<Feature>();
    Feature featureToChange;
    IPropertyType propType;
    List /* nextPropList, */propList;
    Object newProp;

    for( int i = 0; i < changes.length; i++ )
    {
      final FeatureChange change = changes[i];
      featureToChange = change.getFeature();
      propType = change.getProperty();
      if( propType == null )
      {
        System.out.println( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.cmds.ListPropertyChangeCommand.2") + featureToChange ); //$NON-NLS-1$
        continue;
      }
      if( featureToChange == null )
      {
        continue;
      }
      else
      {
        propList = (List) featureToChange.getProperty( propType );
      }
      newProp = change.getNewValue();
      m_oldChanges[i] = new FeatureChange( featureToChange, propType, new ArrayList( propList ) );
      // nextPropList= new ArrayList(propList);
      if( newProp instanceof List )
      {
        // nextPropList.addAll( (List)newProp );
        propList.addAll( (List) newProp );
      }
      else
      {
        // nextPropList.add( newProp );
        propList.add( newProp );
      }
      featureToChange.setProperty( propType, propList/* nextPropList */);
      // FeatureHelper.addProperty(
      // change.getFeature(),
      // change.getProperty(),
      // change.getNewValue() );
      changedFeaturesList.add( change.getFeature() );
    }

    if( m_workspace != null )
    {
      final Feature[] cfs = changedFeaturesList.toArray( new Feature[changedFeaturesList.size()] );
      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, cfs ) );
    }
  }

  private final void undoChanges( )
  {
    // IPropertyType proType;
    List propToRetain;
    List curProp;
    for( FeatureChange change : m_oldChanges )
    {
      if( change != null )
      {
        propToRetain = (List) change.getNewValue();
        curProp = (List) change.getFeature().getProperty( change.getProperty() );
        curProp.retainAll( propToRetain );
      }
    }
  }
}
