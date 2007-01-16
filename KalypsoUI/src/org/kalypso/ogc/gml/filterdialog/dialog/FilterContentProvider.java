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
package org.kalypso.ogc.gml.filterdialog.dialog;

import java.util.ArrayList;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.gml.filterdialog.model.FilterRootElement;
import org.kalypsodeegree.filterencoding.ElseFilter;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;
import org.kalypsodeegree_impl.filterencoding.LogicalOperation;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;

public class FilterContentProvider implements ITreeContentProvider, IPropertyChangeListener
{
  protected Viewer m_viewer = null;

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    try
    {
      if( parentElement instanceof FilterRootElement )
      {
        return ((FilterRootElement) parentElement).getChildren();

      }
      else if( parentElement instanceof ComplexFilter )
      {
        ComplexFilter cf = (ComplexFilter) parentElement;
        Operation operation = cf.getOperation();
        if( operation != null )
        {
          return new Object[] { operation };
        }
        return new Object[0];

      }
      else if( parentElement instanceof FeatureFilter )
      {
        return new Object[0];
      }
      else if( parentElement instanceof ElseFilter )
      {
        return new Object[0];
      }
      else if( parentElement instanceof Filter )
        return new Object[0];
      else if( parentElement instanceof Operation )
      {
        Operation operation = ((Operation) parentElement);
        int operatorId = operation.getOperatorId();
        int typeId = OperationDefines.getTypeById( operatorId );
        if( typeId == OperationDefines.TYPE_SPATIAL )
          return new Object[0];

        if( typeId == OperationDefines.TYPE_LOGICAL )
        {
          ArrayList arguments = ((LogicalOperation) operation).getArguments();
          if( arguments == null )
          {
            return new Object[0];
          }
          return ((LogicalOperation) operation).getArguments().toArray();
        }
        if( typeId == OperationDefines.TYPE_COMPARISON )
        {
          return new Object[0];
        }
      }
      return new Object[0];
    }
    catch( Exception e )
    {
      e.getLocalizedMessage();
      // do noting at the moment
      return new Object[0];
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    return null;
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
    if( inputElement instanceof Object[] )
    {
      Object[] roots = (Object[]) inputElement;
      if( roots.length > 0 )
        return new Object[] { roots[0] };
    }
    return new Object[] {};
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    // nothing
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = viewer;
    if( oldInput != newInput )
    {
      if( oldInput != null )
      {
        if( oldInput instanceof FilterRootElement )
          ((FilterRootElement) oldInput).removeProperyChangeListener( this );
      }
      if( newInput != null && newInput instanceof Object[] )
      {
        Object test = ((Object[]) newInput)[0];
        if( test instanceof FilterRootElement )
          ((FilterRootElement) test).addPropertyChangeListener( this );
      }

    }
  }

  /**
   * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
   */
  public void propertyChange( PropertyChangeEvent event )
  {
    Object source = event.getSource();
    Object newValue = event.getNewValue();
    if( source instanceof FilterRootElement && m_viewer instanceof TreeViewer )
    {
      ((TreeViewer) m_viewer).update( newValue, null );

    }

  }
}