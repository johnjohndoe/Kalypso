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
package org.kalypso.ogc.gml.mapmodel;

import java.net.URL;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.util.command.ICommandManagerListener;

/**
 * Decorator über einen Workspace, der diesen um die Fähigkeiten eines {@link org.kalypso.util.command.ICommandManager ICommandManagers} erweitert
 * 
 * @author belger
 */
public class CommandableWorkspace implements GMLWorkspace, ICommandManager
{
  private final GMLWorkspace m_workspace;
  private final ICommandManager m_commandManager = new DefaultCommandManager();
  
  public CommandableWorkspace( final GMLWorkspace workspace )
  {
    m_workspace = workspace;
  }
  
  public void addCommandManagerListener( ICommandManagerListener l )
  {
    m_commandManager.addCommandManagerListener( l );
  }
  public boolean canRedo()
  {
    return m_commandManager.canRedo();
  }
  public boolean canUndo()
  {
    return m_commandManager.canUndo();
  }
  public String getRedoDescription()
  {
    return m_commandManager.getRedoDescription();
  }
  public String getUndoDescription()
  {
    return m_commandManager.getUndoDescription();
  }
  public void postCommand( ICommand command ) throws Exception
  {
    m_commandManager.postCommand( command );
  }
  public void redo() throws Exception
  {
    m_commandManager.redo();
  }
  public void removeCommandManagerListener( ICommandManagerListener l )
  {
    m_commandManager.removeCommandManagerListener( l );
  }
  public void undo() throws Exception
  {
    m_commandManager.undo();
  }
  public void accept( FeatureVisitor fv, Feature feature, int depth )
  {
    m_workspace.accept( fv, feature, depth );
  }
  public void accept( FeatureVisitor fv, FeatureType ft, int depth )
  {
    m_workspace.accept( fv, ft, depth );
  }
  public void accept( FeatureVisitor fv, List features, int depth )
  {
    m_workspace.accept( fv, features, depth );
  }
  public void addModellListener( ModellEventListener listener )
  {
    m_workspace.addModellListener( listener );
  }
  public void fireModellEvent( ModellEvent event )
  {
    m_workspace.fireModellEvent( event );
  }
  public URL getContext()
  {
    return m_workspace.getContext();
  }
  public Feature getFeature( String id )
  {
    return m_workspace.getFeature( id );
  }
  public Object getFeatureFromPath( String featurePath )
  {
    return m_workspace.getFeatureFromPath( featurePath );
  }
  public Feature[] getFeatures( FeatureType ft )
  {
    return m_workspace.getFeatures( ft );
  }
  public FeatureType getFeatureType( String featureName )
  {
    return m_workspace.getFeatureType( featureName );
  }
  public FeatureType getFeatureTypeFromPath( String featurePath )
  {
    return m_workspace.getFeatureTypeFromPath( featurePath );
  }
  public FeatureType[] getFeatureTypes()
  {
    return m_workspace.getFeatureTypes();
  }
  public Feature getRootFeature()
  {
    return m_workspace.getRootFeature();
  }
  public void removeModellListener( ModellEventListener listener )
  {
    m_workspace.removeModellListener( listener );
  }
  public Feature resolveLink( Feature srcFeature, String linkPropertyName )
  {
    return m_workspace.resolveLink( srcFeature, linkPropertyName );
  }
  public Feature[] resolveLinks( Feature srcFeature, String linkPropertyName )
  {
    return m_workspace.resolveLinks( srcFeature, linkPropertyName );
  }
  public Feature[] resolveWhoLinksTo( Feature linkTargetfeature, FeatureType linkSrcFeatureType,
      String linkPropertyName )
  {
    return m_workspace.resolveWhoLinksTo( linkTargetfeature, linkSrcFeatureType, linkPropertyName );
  }
  public boolean isDirty()
  {
    return m_commandManager.isDirty();
  }
  public void resetDirty()
  {
    m_commandManager.resetDirty();
  }

  public GMLWorkspace getWorkspace()
  {
    return m_workspace;
  }

  public String getFeaturepathForFeature( final Feature feature )
  {
    return m_workspace.getFeaturepathForFeature(feature);
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#getSchemaLocation()
   */
  public String getSchemaLocation()
  {
    return m_workspace.getSchemaLocation();
  }
  
  /**
   * @see org.deegree.model.feature.GMLWorkspace#getSchemaNamespace()
   */
  public String getSchemaNamespace()
  {
    return m_workspace.getSchemaNamespace();
  }
}
