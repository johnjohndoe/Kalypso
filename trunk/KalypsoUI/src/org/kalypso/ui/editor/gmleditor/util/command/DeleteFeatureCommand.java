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
package org.kalypso.ui.editor.gmleditor.util.command;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author belger
 */
public class DeleteFeatureCommand implements ICommand
{
  private final EasyFeatureWrapper[] m_wrappers;

  private final Map m_listIndexMap = new HashMap();

  public DeleteFeatureCommand( final CommandableWorkspace workspace, final Feature parentFeature,
      final String propName, final Feature featureToDelete )
  {
    m_wrappers = new EasyFeatureWrapper[]
    { new EasyFeatureWrapper( workspace, featureToDelete, parentFeature, propName ) };
  }

  public DeleteFeatureCommand( final EasyFeatureWrapper[] wrappers )
  {
    m_wrappers = wrappers;
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
    delete();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    delete();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    final Map parentMap = new HashMap( m_wrappers.length );

    for( int i = 0; i < m_wrappers.length; i++ )
    {
      final EasyFeatureWrapper wrapper = m_wrappers[i];

      final CommandableWorkspace workspace = wrapper.getWorkspace();
      final Feature parentFeature = wrapper.getParentFeature();
      final String propName = wrapper.getParentFeatureProperty();
      final Feature feature = wrapper.getFeature();

      final Object prop = parentFeature.getProperty( propName );
      final Object properties[] = parentFeature.getProperties();
      int propIndex = 0;
      for( ; propIndex < properties.length; propIndex++ )
        if( properties[propIndex] == prop )
          break;

      int maxOccurs = parentFeature.getFeatureType().getMaxOccurs( propIndex );

      if( maxOccurs == 1 )
      {
        parentFeature.setProperty( FeatureFactory.createFeatureProperty(
            propName, feature ) );
      }
      else if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
      {
        final List list = (List)prop;
        
        final Integer index = (Integer)m_listIndexMap.get(wrapper);
        list.add( index.intValue(), feature );
      }
      
      final Object oldParentFeature = parentMap.get( workspace );
      if( oldParentFeature == null )
        parentMap.put( workspace, parentFeature );
      else if( oldParentFeature != parentFeature )
        parentMap.put( workspace, workspace.getRootFeature() );
    }

    for( final Iterator mapIt = parentMap.entrySet().iterator(); mapIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mapIt.next();
      final CommandableWorkspace workspace = (CommandableWorkspace)entry.getKey();
      final Feature parentFeature = (Feature)entry.getValue();

      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature,
          FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature löschen";
  }

  private void delete()
  {
    final Map parentMap = new HashMap( m_wrappers.length );

    for( int i = 0; i < m_wrappers.length; i++ )
    {
      final EasyFeatureWrapper wrapper = m_wrappers[i];

      final CommandableWorkspace workspace = wrapper.getWorkspace();
      final Feature parentFeature = wrapper.getParentFeature();
      final String propName = wrapper.getParentFeatureProperty();
      final Feature feature = wrapper.getFeature();

      final Object prop = parentFeature.getProperty( propName );
      final Object properties[] = parentFeature.getProperties();

      int propIndex = 0;
      for( ; propIndex < properties.length; propIndex++ )
        if( properties[propIndex] == prop )
          break;

      final int maxOccurs = parentFeature.getFeatureType().getMaxOccurs( propIndex );

      if( maxOccurs == 1 )
      {
        parentFeature.setProperty( FeatureFactory.createFeatureProperty(
            propName, null ) );
      }
      else if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
      {
        final List list = (List)prop;
        m_listIndexMap.put( wrapper, new Integer( list.indexOf( feature ) ) );
        list.remove( feature );
      }

      final Object oldParentFeature = parentMap.get( workspace );
      if( oldParentFeature == null )
        parentMap.put( workspace, parentFeature );
      else if( oldParentFeature != parentFeature )
        parentMap.put( workspace, workspace.getRootFeature() );
    }

    for( final Iterator mapIt = parentMap.entrySet().iterator(); mapIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mapIt.next();
      final CommandableWorkspace workspace = (CommandableWorkspace)entry.getKey();
      final Feature parentFeature = (Feature)entry.getValue();

      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature,
          FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
    }
  }
}