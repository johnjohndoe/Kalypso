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

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.commons.command.ICommandManagerListener;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * Decorator über einen Workspace, der diesen um die Fähigkeiten eines
 * {@link org.kalypso.commons.command.ICommandManager ICommandManagers}erweitert
 * 
 * @author belger
 */
public class CommandableWorkspace implements GMLWorkspace, ICommandManager
{
  private final GMLWorkspace_Impl m_workspace;

  private final ICommandManager m_commandManager = new DefaultCommandManager();

  public CommandableWorkspace( final GMLWorkspace workspace )
  {
    /**
     * it does not make sence decorate something else than the real workspace <br>
     * the UML looks also nicer without recursive dependencies here
     */
    m_workspace = (GMLWorkspace_Impl) workspace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#dispose()
   */
  public void dispose( )
  {
    m_workspace.dispose();
  }

  public void addCommandManagerListener( final ICommandManagerListener l )
  {
    m_commandManager.addCommandManagerListener( l );
  }

  public boolean canRedo( )
  {
    return m_commandManager.canRedo();
  }

  public boolean canUndo( )
  {
    return m_commandManager.canUndo();
  }

  public String getRedoDescription( )
  {
    return m_commandManager.getRedoDescription();
  }

  public String getUndoDescription( )
  {
    return m_commandManager.getUndoDescription();
  }

  /**
   * @see org.kalypso.commons.command.ICommandManager#postCommand(org.kalypso.commons.command.ICommand)
   */
  public void postCommand( final ICommand command ) throws Exception
  {
    m_commandManager.postCommand( command );
  }

  public void redo( ) throws Exception
  {
    m_commandManager.redo();
  }

  public void removeCommandManagerListener( final ICommandManagerListener l )
  {
    m_commandManager.removeCommandManagerListener( l );
  }

  public void undo( ) throws Exception
  {
    m_commandManager.undo();
  }

  public void accept( final FeatureVisitor fv, final Feature feature, final int depth )
  {
    m_workspace.accept( fv, feature, depth );
  }

  public void accept( final FeatureVisitor fv, final IFeatureType ft, final int depth )
  {
    m_workspace.accept( fv, ft, depth );
  }

  public void accept( final FeatureVisitor fv, final List features, final int depth )
  {
    m_workspace.accept( fv, features, depth );
  }

  public void addModellListener( final ModellEventListener listener )
  {
    m_workspace.addModellListener( listener );
  }

  public void fireModellEvent( final ModellEvent event )
  {
    m_workspace.fireModellEvent( event );
  }

  public URL getContext( )
  {
    return m_workspace.getContext();
  }

  public Feature getFeature( final String id )
  {
    return m_workspace.getFeature( id );
  }

  public Object getFeatureFromPath( final String featurePath )
  {
    return m_workspace.getFeatureFromPath( featurePath );
  }

  public Feature[] getFeatures( final IFeatureType ft )
  {
    return m_workspace.getFeatures( ft );
  }

  public IFeatureType getFeatureType( final QName featureName )
  {
    return m_workspace.getFeatureType( featureName );
  }

  public IFeatureType getFeatureTypeFromPath( final String featurePath )
  {
    return m_workspace.getFeatureTypeFromPath( featurePath );
  }

  public Feature getRootFeature( )
  {
    return m_workspace.getRootFeature();
  }

  public void removeModellListener( final ModellEventListener listener )
  {
    m_workspace.removeModellListener( listener );
  }

  public Feature resolveLink( final Feature srcFeature, final IRelationType linkPropertyName, final int resolveMode )
  {
    return m_workspace.resolveLink( srcFeature, linkPropertyName, resolveMode );
  }

  public Feature resolveLink( final Feature srcFeature, final IRelationType linkPropertyName )
  {
    return m_workspace.resolveLink( srcFeature, linkPropertyName );
  }

  public Feature[] resolveLinks( final Feature srcFeature, final IRelationType linkPropertyName )
  {
    return m_workspace.resolveLinks( srcFeature, linkPropertyName );
  }

  public Feature[] resolveLinks( final Feature srcFeature, final IRelationType linkPropertyName, final int resolveMode )
  {
    return m_workspace.resolveLinks( srcFeature, linkPropertyName, resolveMode );
  }

  public Feature[] resolveWhoLinksTo( final Feature linkTargetfeature, final IFeatureType linkSrcFeatureType, final IRelationType linkPropertyName )
  {
    return m_workspace.resolveWhoLinksTo( linkTargetfeature, linkSrcFeatureType, linkPropertyName );
  }

  public boolean isDirty( )
  {
    return m_commandManager.isDirty();
  }

  public void resetDirty( )
  {
    m_commandManager.resetDirty();
  }

  public GMLWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public FeaturePath getFeaturepathForFeature( final Feature feature )
  {
    return m_workspace.getFeaturepathForFeature( feature );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getSchemaLocation()
   */
  public String getSchemaLocationString( )
  {
    return m_workspace.getSchemaLocationString();
  }

  public Feature createFeature( final Feature parent, final IRelationType parentRelation, final IFeatureType type )
  {
    return m_workspace.createFeature( parent, parentRelation, type );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#createFeature(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.feature.IFeatureType, int)
   */
  public Feature createFeature( final Feature parent, final IRelationType parentRelation, final IFeatureType type, final int depth )
  {
    return m_workspace.createFeature( parent, parentRelation, type, depth );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#addFeatureAsComposition(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, org.kalypsodeegree.model.feature.Feature)
   */
  public void addFeatureAsComposition( final Feature parent, final IRelationType propName, final int pos, final Feature newFeature ) throws Exception
  {
    m_workspace.addFeatureAsComposition( parent, propName, pos, newFeature );
  }

  public void addFeatureAsAggregation( final Feature parent, final IRelationType propName, final int pos, final String featureID ) throws Exception
  {
    m_workspace.addFeatureAsAggregation( parent, propName, pos, featureID );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#setFeatureAsAggregation(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, java.lang.String)
   */
  public void setFeatureAsAggregation( final Feature srcFE, final IRelationType propName, final int pos, final String featureID ) throws Exception
  {
    m_workspace.setFeatureAsAggregation( srcFE, propName, pos, featureID );
  }

  public boolean removeLinkedAsAggregationFeature( final Feature parentFeature, final IRelationType propName, final String linkFeatureId )
  {
    return m_workspace.removeLinkedAsAggregationFeature( parentFeature, propName, linkFeatureId );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsCompositionFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeLinkedAsCompositionFeature( final Feature parentFeature, final IRelationType propName, final Feature childFeature )
  {
    return m_workspace.removeLinkedAsCompositionFeature( parentFeature, propName, childFeature );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      java.lang.String, int)
   */
  public void accept( final FeatureVisitor fv, final String featurePath, final int depth )
  {
    m_workspace.accept( fv, featurePath, depth );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#isExistingRelation(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.Feature, java.lang.String)
   */
  public boolean isExistingRelation( final Feature f1, final Feature f2, final IRelationType name )
  {
    return m_workspace.isExistingRelation( f1, f2, name );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#isAggrigatedLink(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int)
   */
  public boolean isAggregatedLink( final Feature parent, final IRelationType linkPropName, final int pos )
  {
    return m_workspace.isAggregatedLink( parent, linkPropName, pos );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#setFeatureAsComposition(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature, boolean)
   */
  public void setFeatureAsComposition( final Feature parentFE, final IRelationType linkPropName, final Feature linkedFE, final boolean overwrite ) throws Exception
  {
    m_workspace.setFeatureAsComposition( parentFE, linkPropName, linkedFE, overwrite );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#setFeatureAsAggregation(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, java.lang.String, boolean)
   */
  public void setFeatureAsAggregation( final Feature parent, final IRelationType propName, final String featureID, final boolean overwrite ) throws Exception
  {
    m_workspace.setFeatureAsAggregation( parent, propName, featureID, overwrite );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      org.kalypsodeegree.model.feature.Feature, int, org.kalypsodeegree.model.feature.IPropertyType[])
   */
  public void accept( final FeatureVisitor visitor, final Feature feature, final int depth, final IPropertyType[] featureProperties )
  {
    m_workspace.accept( visitor, feature, depth, featureProperties );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#contains(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean contains( final Feature feature )
  {
    return m_workspace.contains( feature );
  }

  /**
   * @param parentFeature
   * @param ftp
   * @param pos
   * @return
   */
  public boolean isBrokenLink( final Feature parentFeature, final IPropertyType ftp, final int pos )
  {
    return m_workspace.isBrokenLink( parentFeature, ftp, pos );
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureType(java.lang.String)
   */
  @Deprecated
  public IFeatureType getFeatureType( final String nameLocalPart )
  {
    return m_workspace.getFeatureType( nameLocalPart );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getGMLSchema()
   */
  public IGMLSchema getGMLSchema( )
  {
    return m_workspace.getGMLSchema();
  }

  public ICommandManager getCommandManager( )
  {
    return m_commandManager;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl#getFeatureProviderFactory()
   */
  public IFeatureProviderFactory getFeatureProviderFactory( )
  {
    return m_workspace.getFeatureProviderFactory();
  }

  /**
   * @see org.kalypso.commons.command.ICommandManager#clear()
   */
  public void clear( )
  {
    m_commandManager.clear();
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getNamespaceContext()
   */
  public NamespaceContext getNamespaceContext( )
  {
    return m_workspace.getNamespaceContext();
  }

  public void setSchemaLocation( String schemaLocation )
  {
    m_workspace.setSchemaLocation( schemaLocation );
  }
}