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
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * @author Gernot Belger
 * @author Monika Thül
 */
public class DeleteFeatureCommand implements ICommand
{
  private final EasyFeatureWrapper[] m_wrappers;

  private final Map<EasyFeatureWrapper, Integer> m_listIndexMap = new HashMap<EasyFeatureWrapper, Integer>();

  final List<RemoveBrokenLinksCommand> m_removeBrokenLinksCommands = new ArrayList<RemoveBrokenLinksCommand>();

  public DeleteFeatureCommand( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentProp, final Feature featureToDelete )
  {
    this( new EasyFeatureWrapper[] { new EasyFeatureWrapper( workspace, featureToDelete, parentFeature, parentProp ) } );
  }

  public DeleteFeatureCommand( final EasyFeatureWrapper[] wrappers )
  {
    m_wrappers = wrappers;
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
    delete();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    delete();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    final Map<Feature, List<Feature>> parentMap = new HashMap<Feature, List<Feature>>();

    for( final EasyFeatureWrapper wrapper : m_wrappers )
    {
      final CommandableWorkspace workspace = wrapper.getWorkspace();
      final Feature parentFeature = wrapper.getParentFeature();
      final IRelationType rt = wrapper.getParentFeatureProperty();
      final Feature featureToAdd = wrapper.getFeature();

      if( workspace.contains( featureToAdd ) )
        continue;

      if( rt.isList() )
      {
        final int index = (m_listIndexMap.get( wrapper )).intValue();
        workspace.addFeatureAsComposition( parentFeature, rt, index, featureToAdd );
      }
      else
        workspace.addFeatureAsComposition( parentFeature, rt, 0, featureToAdd );

      // collect infos for event
      if( !parentMap.containsKey( parentFeature ))
        parentMap.put( parentFeature, new ArrayList<Feature>() );
      
      final List<Feature> children = parentMap.get( parentFeature );
      children.add( featureToAdd );
    }

    /* fire modell events */
    for( final Map.Entry<Feature, List<Feature>> entry : parentMap.entrySet() )
    {
      final Feature parentFeature = entry.getKey();
      final List<Feature> childList = entry.getValue();
      final Feature[] children = childList.toArray( new Feature[childList.size()] );
      final GMLWorkspace workspace = parentFeature.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, children, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    }

    for( final ICommand command : m_removeBrokenLinksCommands )
      command.undo();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Feature löschen";
  }

  private void delete( ) throws Exception
  {
    m_removeBrokenLinksCommands.clear();

    final Set<GMLWorkspace> touchedWorkspaces = new HashSet<GMLWorkspace>();

    // collect event information
    final Map<Feature, List<Feature>> parentMap = new HashMap<Feature, List<Feature>>();
    
    for( final EasyFeatureWrapper wrapper : m_wrappers )
    {
      final CommandableWorkspace workspace = wrapper.getWorkspace();
      touchedWorkspaces.add( workspace );
      final Feature parentFeature = wrapper.getParentFeature();
      final IRelationType rt = wrapper.getParentFeatureProperty();
      final Feature featureToRemove = wrapper.getFeature();
      if( !workspace.contains( featureToRemove ) )
        continue; // is allready remved

      if( rt.isList() )
      {
        final Object prop = parentFeature.getProperty( rt );
        final List list = (List) prop;
        m_listIndexMap.put( wrapper, new Integer( list.indexOf( featureToRemove ) ) );
      }
      // remove the feature
      workspace.removeLinkedAsCompositionFeature( parentFeature, rt, featureToRemove );

      // collect infos for event
      if( !parentMap.containsKey( parentFeature ))
        parentMap.put( parentFeature, new ArrayList<Feature>() );
      
      final List<Feature> children = parentMap.get( parentFeature );
      children.add( featureToRemove );
    }

    /* fire modell events */
    for( final Map.Entry<Feature, List<Feature>> entry : parentMap.entrySet() )
    {
      final Feature parentFeature = entry.getKey();
      final List<Feature> childList = entry.getValue();
      final Feature[] children = childList.toArray( new Feature[childList.size()] );
      final GMLWorkspace workspace = parentFeature.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, children, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
    }

    for( final GMLWorkspace workspace : touchedWorkspaces )
    {
      final FeatureVisitor visitor = new FeatureVisitor()
      {
        // checks all properties for broken links
        /**
         * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
         */
        public boolean visit( Feature f )
        {
          final IFeatureType ft = f.getFeatureType();
          final IPropertyType[] ftps = ft.getProperties();
          for( int j = 0; j < ftps.length; j++ )
          {
            if( ftps[j] instanceof IRelationType )
            {
              IRelationType linkftp = (IRelationType) ftps[j];
              if( linkftp.isList() )
              {
                final List propList = (List) f.getProperty( linkftp );
                // important: count down not up
                for( int k = propList.size() - 1; k >= 0; k-- )
                {
                  if( workspace.isBrokenLink( f, linkftp, k ) )
                    m_removeBrokenLinksCommands.add( new RemoveBrokenLinksCommand( workspace, f, linkftp, (String) propList.get( k ), k ) );
                }
              }
              else
              {
                if( workspace.isBrokenLink( f, linkftp, 1 ) )
                {
                  String childID = (String) f.getProperty( linkftp );
                  m_removeBrokenLinksCommands.add( new RemoveBrokenLinksCommand( workspace, f, linkftp, childID, 1 ) );
                }
              }
            }
          }
          return true;
        }
      };
      workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    }

    for( final ICommand command : m_removeBrokenLinksCommands )
      command.process();
  }

  private static class RemoveBrokenLinksCommand implements ICommand
  {
    private final GMLWorkspace m_workspace;

    private final Feature m_parentFeature;

    private final String m_childID;

    private final IRelationType m_ftp;

    private final int m_pos;

    public RemoveBrokenLinksCommand( final GMLWorkspace workspace, final Feature parentFeature, final IRelationType ftp, final String childID, final int pos )
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
    public boolean isUndoable( )
    {
      return true;
    }

    /**
     * @see org.kalypso.commons.command.ICommand#process()
     */
    public void process( ) throws Exception
    {
      m_workspace.removeLinkedAsAggregationFeature( m_parentFeature, m_ftp, m_childID );

      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, new Feature[] { m_parentFeature } ) );
    }

    /**
     * @see org.kalypso.commons.command.ICommand#redo()
     */
    public void redo( ) throws Exception
    {
      process();
    }

    /**
     * @see org.kalypso.commons.command.ICommand#undo()
     */
    public void undo( ) throws Exception
    {
      m_workspace.addFeatureAsAggregation( m_parentFeature, m_ftp, m_pos, m_childID );

      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, new Feature[] { m_parentFeature } ) );
    }

    /**
     * @see org.kalypso.commons.command.ICommand#getDescription()
     */
    public String getDescription( )
    {
      // egal
      return null;
    }

  }
}