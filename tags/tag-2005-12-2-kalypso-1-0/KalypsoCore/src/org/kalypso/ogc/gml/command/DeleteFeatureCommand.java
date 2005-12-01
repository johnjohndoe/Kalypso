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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author belger
 */
public class DeleteFeatureCommand implements ICommand
{
  private final EasyFeatureWrapper[] m_wrappers;

  private final Map m_listIndexMap = new HashMap();

  final List m_removeBrokenLinksCommands = new ArrayList();

  private final Set m_touchedWorkspaces = new HashSet();

  public DeleteFeatureCommand( final CommandableWorkspace workspace, final Feature parentFeature,
      final String propName, final Feature featureToDelete )
  {
    this( new EasyFeatureWrapper[]
    { new EasyFeatureWrapper( workspace, featureToDelete, parentFeature, propName ) } );
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
      final Feature featureToAdd = wrapper.getFeature();

      if( workspace.contains( featureToAdd ) )
        continue;
      //      final Object properties[] = parentFeature.getProperties();
      //      int propIndex = 0;
      //      for( ; propIndex < properties.length; propIndex++ )
      //        if( properties[propIndex] == prop )
      //          break;

      int maxOccurs = parentFeature.getFeatureType().getMaxOccurs( propName );

      //      final Object prop = parentFeature.getProperty( propName );
      if( maxOccurs == 1 )
        workspace.addFeatureAsComposition( parentFeature, propName, 0, featureToAdd );
      else if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
      {
        final int index = ( (Integer)m_listIndexMap.get( wrapper ) ).intValue();
        workspace.addFeatureAsComposition( parentFeature, propName, index, featureToAdd );
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
    for( Iterator iter = m_removeBrokenLinksCommands.iterator(); iter.hasNext(); )
    {
      final ICommand command = (ICommand)iter.next();
      command.undo();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Feature löschen";
  }

  private void delete() throws Exception
  {
    m_removeBrokenLinksCommands.clear();
    m_touchedWorkspaces.clear();
    // collect event information
    final Map parentMap = new HashMap( m_wrappers.length ); // key: workspace / value: parentFeature

    for( int i = 0; i < m_wrappers.length; i++ )
    {
      final EasyFeatureWrapper wrapper = m_wrappers[i];
      final CommandableWorkspace workspace = wrapper.getWorkspace();
      m_touchedWorkspaces.add( workspace );
      final Feature parentFeature = wrapper.getParentFeature();
      final String propName = wrapper.getParentFeatureProperty();
      final Feature featureToRemove = wrapper.getFeature();
      if( !workspace.contains( featureToRemove ) )
        continue; // is allready remved

      // remember position for undo
      final int maxOccurs = parentFeature.getFeatureType().getMaxOccurs( propName );
      if( maxOccurs > 1 || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
      {
        final Object prop = parentFeature.getProperty( propName );
        final List list = (List)prop;
        m_listIndexMap.put( wrapper, new Integer( list.indexOf( featureToRemove ) ) );
      }
      // remove the feature
      workspace.removeLinkedAsCompositionFeature( parentFeature, propName, featureToRemove );

      // collect infos for event
      final Object oldParentFeature = parentMap.get( workspace );
      if( oldParentFeature == null )
        parentMap.put( workspace, parentFeature );
      else if( oldParentFeature != parentFeature )
        parentMap.put( workspace, workspace.getRootFeature() );
    }
    // throw event
    for( final Iterator mapIt = parentMap.entrySet().iterator(); mapIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)mapIt.next();
      final CommandableWorkspace workspace = (CommandableWorkspace)entry.getKey();
      final Feature parentFeature = (Feature)entry.getValue();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature,
          FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
    }

    for( Iterator iter = m_touchedWorkspaces.iterator(); iter.hasNext(); )
    {
      final GMLWorkspace workspace = (GMLWorkspace)iter.next();

      final FeatureVisitor visitor = new FeatureVisitor()
      {
        // checks all properties for broken links
        /**
         * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
         */
        public boolean visit( Feature f )
        {
          final FeatureType ft = f.getFeatureType();
          final FeatureTypeProperty[] ftps = ft.getProperties();
          for( int j = 0; j < ftps.length; j++ )
          {
            FeatureTypeProperty linkftp = ftps[j];
            if( linkftp instanceof FeatureAssociationTypeProperty )
            {
              if( ft.getMaxOccurs( linkftp.getName() ) == 1 )
              {
                if( workspace.isBrokenLink( f, linkftp, 1 ) )
                {
                  String childID = (String)f.getProperty( linkftp.getName() );
                  m_removeBrokenLinksCommands.add( new RemoveBrokenLinksCommand( workspace, f, linkftp, childID, 1 ) );
                }
              }
              else
              {
                final List propList = (List)f.getProperty( linkftp.getName() );
                // important: count down not up
                for( int k = propList.size() - 1; k >= 0; k-- )
                {
                  if( workspace.isBrokenLink( f, linkftp, k ) )
                  {
                    m_removeBrokenLinksCommands.add( new RemoveBrokenLinksCommand( workspace, f, linkftp,
                        (String)propList.get( k ), k ) );
                  }
                }
              }
            }
          }
          return true;
        }
      };
      workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    }
    for( Iterator iter = m_removeBrokenLinksCommands.iterator(); iter.hasNext(); )
    {
      final ICommand command = (ICommand)iter.next();
      command.process();
    }
  }

  class RemoveBrokenLinksCommand implements ICommand
  {

    private final GMLWorkspace m_workspace;

    private final Feature m_parentFeature;

    private final String m_childID;

    private final FeatureTypeProperty m_ftp;

    private final int m_pos;

    public RemoveBrokenLinksCommand( final GMLWorkspace workspace, final Feature parentFeature,
        final FeatureTypeProperty ftp, final String childID, final int pos )
    {
      m_workspace = workspace;
      m_parentFeature = parentFeature;
      m_ftp = ftp;
      m_childID = childID;
      m_pos = pos;
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
      m_workspace.removeLinkedAsAggregationFeature( m_parentFeature, m_ftp.getName(), m_childID );
    }

    /**
     * @see org.kalypso.commons.command.ICommand#redo()
     */
    public void redo() throws Exception
    {
      process();
    }

    /**
     * @see org.kalypso.commons.command.ICommand#undo()
     */
    public void undo() throws Exception
    {
      m_workspace.addFeatureAsAggregation( m_parentFeature, m_ftp.getName(), m_pos, m_childID );
    }

    /**
     * @see org.kalypso.commons.command.ICommand#getDescription()
     */
    public String getDescription()
    {
      // egal
      return null;
    }

  }
}