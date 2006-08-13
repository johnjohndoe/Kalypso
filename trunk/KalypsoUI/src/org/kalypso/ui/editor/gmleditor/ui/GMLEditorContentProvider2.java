/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.tree.FindParentTreeVisitor;
import org.kalypso.contribs.eclipse.jface.viewers.tree.TreeViewerUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class GMLEditorContentProvider2 implements ITreeContentProvider
{
  private CommandableWorkspace m_workspace;

  /**
   * remebers the child-parent relationship. This is nedded, because if we provide no parent, setExpandedElements
   * doesn't work, which will lead to an unuseable gui.
   */
  private final Map<Object, Object> m_parentHash = new HashMap<Object, Object>();

  private TreeViewer m_viewer;

  /**
   * The x-path to the currently (not visible) root element. Its children are the root elements of the tree.
   * <p>
   * If null, the root-feature is the root element of the tree.
   */
  private GMLXPath m_rootPath = new GMLXPath( "" );

  /**
   * Gets the children and updates the parent-hash.
   * 
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    final Object[] children = getChildrenInternal( parentElement );
    if( children == null )
      return null;

    for( int i = 0; i < children.length; i++ )
      m_parentHash.put( children[i], parentElement );

    return children;
  }

  private Object[] getChildrenInternal( final Object parentElement )
  {
    final List<Object> result = new ArrayList<Object>();
    if( parentElement instanceof GMLWorkspace )
    {
      return new Object[] { ((GMLWorkspace) parentElement).getRootFeature() };
    }
    if( parentElement instanceof Feature )
    {
      Feature parentFE = (Feature) parentElement;
      final IPropertyType[] properties = parentFE.getFeatureType().getProperties();

      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType property = properties[i];
        if( property instanceof IRelationType )
          result.add( new FeatureAssociationTypeElement( (Feature) parentElement, (IRelationType) property ) );

        if( GeometryUtilities.isGeometry( property ) )
        {
          final Object value = parentFE.getProperty( property );
          if( value != null )
            result.add( value );
        }
      }
      return result.toArray();
    }
    if( parentElement instanceof FeatureAssociationTypeElement )
    {
      final Feature parentFeature = ((FeatureAssociationTypeElement) parentElement).getParentFeature();
      final IRelationType ftp = ((FeatureAssociationTypeElement) parentElement).getAssociationTypeProperty();
      final Feature[] features = m_workspace.resolveLinks( parentFeature, ftp );
      for( int i = 0; i < features.length; i++ )
      {
        final Feature feature = features[i];
        if( feature != null )
        {
          if( m_workspace.isAggregatedLink( parentFeature, ftp, i ) )
            result.add( new LinkedFeatureElement2( feature ) );
          else
            result.add( feature );
        }
      }
      return result.toArray();
    }
    // this should never happen
    return result.toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    final Object object = m_parentHash.get( element );
    if( object != null )
      return object;

    // brute force search TODO: not always successfull, because the tree
    // may now start at an arbitrary element.
    // We should combine with a search throug the GMLWorkspace
    final FindParentTreeVisitor visitor = new FindParentTreeVisitor( element );
    TreeViewerUtilities.accept( m_viewer, visitor );

    final Object parent = visitor.getParent();
    
    // Something like that, but this wont work
//    if( parent == null && parent instanceof Feature )
//      ((Feature)parent).getParent();
    
    if( parent != null )
      m_parentHash.put( element, parent );

    return parent;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    if( element == null )
      return false;
    return getChildren( element ).length > 0;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( m_workspace == null )
      return new Object[] {};
    
    final Object[] rootFeatureObjects = new Object[] { m_workspace.getRootFeature() };

    try
    {
      final Object object = GMLXPathUtilities.query( m_rootPath, m_workspace );
      if( object == m_workspace )
        return rootFeatureObjects;

      return getChildren( object );
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );

      return rootFeatureObjects;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    m_parentHash.clear();
    m_viewer = null;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = (TreeViewer) viewer;

    if( oldInput != newInput )
    {
      m_parentHash.clear();

      if( oldInput != null )
        m_workspace = null;

      if( newInput instanceof CommandableWorkspace )
        m_workspace = (CommandableWorkspace) newInput;
      else
        m_workspace = null;

      m_rootPath = new GMLXPath( "" );
    }
  }

  public Feature getParentFeature( final Feature feature )
  {
    final FeatureAssociationTypeElement parent = (FeatureAssociationTypeElement) getParent( feature );
    if( parent == null )
      return null;

    return parent.getParentFeature();
  }

  public IRelationType getParentFeatureProperty( final Feature feature )
  {
    final FeatureAssociationTypeElement parent = (FeatureAssociationTypeElement) getParent( feature );
    if( parent == null )
      return null;

    return parent.getAssociationTypeProperty();
  }

  /** Expand the element and all of its parents */
  public void expandElement( final Object element )
  {
    if( element == null )
      return;

    expandElement( getParent( element ) );

    m_viewer.setExpandedState( element, true );
  }

  public void goInto( final Object object )
  {
    final Object[] expandedElements = m_viewer.getExpandedElements();

    // prepare for exception
    m_rootPath = new GMLXPath( "" );

    try
    {
      if( object instanceof Feature )
      {
        final Feature feature = (Feature) object;
        m_rootPath = new GMLXPath( feature );
      }
      // REMARK: does not work at the moment, as GMLXPath does not support properties
      else if( object instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) object;
        final Feature parentFeature = fate.getParentFeature();
        final GMLXPath path = new GMLXPath( parentFeature );
        m_rootPath = new GMLXPath( path, fate.getAssociationTypeProperty().getQName().toString() );
      }
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }
    finally
    {
      m_viewer.refresh();
      m_viewer.setExpandedElements( expandedElements );
    }
  }

  public void goUp( )
  {
    final Object[] expandedElements = m_viewer.getExpandedElements();

    try
    {
      final Object object = GMLXPathUtilities.query( m_rootPath, m_workspace );

      final Object parent = getParent( object );
      if( parent != null )
      {
        final Object parent2 = getParent( parent );
        goInto( parent2 );
        return; // do not refresh, we already did so
      }
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }

    m_rootPath = new GMLXPath( "" );

    m_viewer.setExpandedElements( expandedElements );
    m_viewer.refresh();
  }

  public boolean canGoUp( )
  {
    return m_rootPath.getSegmentSize() > 0;
  }

  public GMLXPath getRootPath( )
  {
    return m_rootPath;
  }

  /**
   * Sets the root path and refreshes the tree.
   * <p>
   * Tries to keep the current expansion state.
   * </p>
   */
  public void setRootPath( final GMLXPath rootPath )
  {
    final Object[] expandedElements = m_viewer == null ? null : m_viewer.getExpandedElements();

    try
    {
      m_rootPath = rootPath;
    }
    finally
    {
      if( m_viewer != null )
      {
        m_viewer.setExpandedElements( expandedElements );
        m_viewer.refresh();
      }
    }
  }
}
