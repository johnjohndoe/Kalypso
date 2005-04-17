package org.kalypso.ogc.gml.map.widgets.editrelation;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.eclipse.jface.ITreeVisitor;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

public class CopyOfEditRelationOptionsContentProvider implements ITreeContentProvider
{

  private final Hashtable m_childCache = new Hashtable();

  private final Hashtable m_parentCache = new Hashtable();

  private final HashSet m_checkedElements = new HashSet();

  /*
   * 
   * @author doemming
   */
  public CopyOfEditRelationOptionsContentProvider()
  {
  // nothing
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    if( m_childCache.containsKey( parentElement ) )
      return (Object[])m_childCache.get( parentElement );
    final List result = new ArrayList();
    if( parentElement == null )
      return new Object[0];

    if( parentElement instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)parentElement;
      final CommandableWorkspace workspace = featureTheme.getWorkspace();
      if( workspace != null )
        result.add( workspace );
    }
    if( parentElement instanceof GMLWorkspace )
    {
      final FeatureType[] featureTypes = ( (GMLWorkspace)parentElement ).getFeatureTypes();

      for( int i = 0; i < featureTypes.length; i++ )
      {
        FeatureType ft = featureTypes[i];
        if( ft.getDefaultGeometryProperty() != null )
          result.add( ft );
      }
    }

    if( parentElement instanceof FeatureType )
    {
      FeatureType ft1 = (FeatureType)parentElement;
      FeatureTypeProperty[] properties = ft1.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        FeatureTypeProperty property = properties[i];
        if( property instanceof FeatureAssociationTypeProperty )
        {
          // test is heavy relationship
          FeatureAssociationTypeProperty linkFTP1 = (FeatureAssociationTypeProperty)property;
          FeatureType[] associationFeatureTypes = linkFTP1.getAssociationFeatureTypes();
          for( int j = 0; j < associationFeatureTypes.length; j++ )
          {
            FeatureType ft2 = associationFeatureTypes[j];
            // leight: FT,Prop,FT
            // heavy: FT,Prop,FT,PropFT
            // leight relationship ?
            if( ft2.getDefaultGeometryProperty() != null )
            {
              result.add( new RelationType( ft1, linkFTP1, ft2 ) );
            }
            // heavy relationship?
            else
            {
              FeatureTypeProperty[] properties2 = ft2.getProperties();
              for( int l = 0; l < properties2.length; l++ )
              {
                FeatureTypeProperty property2 = properties2[l];
                if( property2 instanceof FeatureAssociationTypeProperty )
                {
                  FeatureAssociationTypeProperty linkFTP2 = (FeatureAssociationTypeProperty)property2;
                  FeatureType[] associationFeatureTypes2 = linkFTP2.getAssociationFeatureTypes();
                  for( int k = 0; k < associationFeatureTypes2.length; k++ )
                  {
                    final FeatureType ft3 = associationFeatureTypes2[k];
                    if( ft3.getDefaultGeometryProperty() != null )
                    {
                      // it is a heavy relationship;
                      result.add( new HeavyRelationType( ft1, linkFTP1, ft2, linkFTP2, ft3 ) );
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    final Object[] array = result.toArray();
    if( array.length > 0 )
    {
      m_childCache.put( parentElement, array );
      for( int i = 0; i < array.length; i++ )
        m_parentCache.put( array[i], parentElement );
    }
    if( m_childCache.containsKey( parentElement ) )
      return (Object[])m_childCache.get( parentElement );
    return new Object[0];
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( Object element )
  {
    if( m_parentCache.containsKey( element ) )
    {
      return m_parentCache.get( element );
    }
    // can not compute parent
    return null;
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
    if( inputElement != null && inputElement instanceof IKalypsoFeatureTheme )
    {
      IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)inputElement;
      return new GMLWorkspace[]
      { featureTheme.getWorkspace() };
    }
    return new Object[0];
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  // nothing to do
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
   *      java.lang.Object, java.lang.Object)
   */
  public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
  {
    m_childCache.clear();
    m_parentCache.clear();
    m_checkedElements.clear();
    // nothing to do
  }

  public void accept( Object element, ITreeVisitor visitor )
  {
    if( visitor.visit( element, this ) )
    {
      Object[] children = getChildren( element );
      for( int i = 0; i < children.length; i++ )
        accept( children[i], visitor );
    }
  }

  public boolean isChecked( Object element )
  {
    return m_checkedElements.contains( element );
  }

  public void setChecked( Object element, boolean checked )
  {
    if( checked )
      m_checkedElements.add( element );
    else
      m_checkedElements.remove( element );
  }

  public RelationType[] getCheckedRelations()
  {
    final List result = new ArrayList();
    for( Iterator iter = m_checkedElements.iterator(); iter.hasNext(); )
    {
      Object element = iter.next();
      if( element instanceof RelationType )
        result.add( element );
    }
    return (RelationType[])result.toArray( new RelationType[result.size()] );
  }
}