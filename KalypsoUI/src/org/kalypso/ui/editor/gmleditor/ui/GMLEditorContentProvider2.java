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

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kuepfer
 */
public class GMLEditorContentProvider2 implements ITreeContentProvider
{
  private CommandableWorkspace m_workspace;

  /**
   * remebers the child-parent relationship. This is nedded, because if we provide no parent,
   * setExpandedElements doesn't work, which
   * will lead to an unusable gui.
   */
  private Map m_parentHash = new HashMap();

  private TreeViewer m_viewer;

  /**
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
    final List result = new ArrayList();
    if( parentElement instanceof GMLWorkspace )
    {
      return new Object[]
      { ( (GMLWorkspace)parentElement ).getRootFeature() };
    }
    if( parentElement instanceof Feature )
    {
      final FeatureTypeProperty[] properties = ( (Feature)parentElement ).getFeatureType().getProperties();

      for( int i = 0; i < properties.length; i++ )
      {
        final FeatureTypeProperty property = properties[i];
        if( property instanceof FeatureAssociationTypeProperty )
          result.add( new FeatureAssociationTypeElement( (Feature)parentElement,
              (FeatureAssociationTypeProperty)property ) );
      }
      return result.toArray();
    }
    if( parentElement instanceof FeatureAssociationTypeElement )
    {
      final Feature parentFeature = ( (FeatureAssociationTypeElement)parentElement ).getParentFeature();
      final FeatureAssociationTypeProperty ftp = ( (FeatureAssociationTypeElement)parentElement )
          .getAssociationTypeProperty();
      final Feature[] features = m_workspace.resolveLinks( parentFeature, ftp.getName() );
      for( int i = 0; i < features.length; i++ )
      {
        final Feature feature = features[i];
        if( feature != null )
        {
          if( m_workspace.isAggrigatedLink( parentFeature, ftp.getName(), i ) )
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
    if( object == null )
      System.out.println( "No parent for object: " + element );
    
    return object;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( Object element )
  {
    if( element == null )
      return false;
    return getChildren( element ).length > 0;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    if( inputElement instanceof GMLWorkspace )
      return new Object[]
      { ( (GMLWorkspace)inputElement ).getRootFeature() };
    return new Object[0];
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
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
    m_viewer = (TreeViewer)viewer;
    
    if( oldInput != newInput )
    {
      m_parentHash.clear();

      if( oldInput != null )
        m_workspace = null;

      if( newInput instanceof CommandableWorkspace )
        m_workspace = (CommandableWorkspace)newInput;
      else
        m_workspace = null;
    }
  }

  public Feature getParentFeature( final Feature feature )
  {
    final FeatureAssociationTypeElement parent = (FeatureAssociationTypeElement)getParent( feature );
    if( parent == null )
      return null;
    
    return parent.getParentFeature();
  }

  public String getParentFeatureProperty( final Feature feature )
  {
    final FeatureAssociationTypeElement parent = (FeatureAssociationTypeElement)getParent( feature );
    if( parent == null )
      return null;
    
    return parent.getAssociationTypeProperty().getName();
  }

  public void expandElement( final Object element )
  {
    if( element == null )
      return;
    
    expandElement( getParent( element ) );

    m_viewer.setExpandedState( element, true );
  }

}
