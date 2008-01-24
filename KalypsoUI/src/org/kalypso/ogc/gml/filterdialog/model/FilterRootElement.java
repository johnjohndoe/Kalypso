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
package org.kalypso.ogc.gml.filterdialog.model;

import java.util.List;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.ListenerList;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.LogicalOperation;

/**
 * @author kuepfer
 */
public class FilterRootElement implements Filter
{
  private transient ListenerList m_listeners = null;

  private final String m_name = "FILTER"; //$NON-NLS-1$

  private Filter m_filter = null;

  /**
   * @see org.kalypsodeegree.filterencoding.Filter#evaluate(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean evaluate( final Feature feature ) throws FilterEvaluationException
  {
    return m_filter.evaluate( feature );
  }

  /**
   * @see org.kalypsodeegree.filterencoding.Filter#toXML()
   */
  public StringBuffer toXML( )
  {
    return m_filter.toXML();
  }

  public String getName( )
  {
    return m_name;
  }

  public Object[] getChildren( )
  {
    if( m_filter != null )
      return new Object[] { m_filter };

    return new Object[0];
  }

  public void addChild( final Object child )
  {
    if( child instanceof Filter )
      m_filter = (Filter) child;
  }

  public void addPropertyChangeListener( final IPropertyChangeListener listener )
  {
    getPropetyChangedListeners().add( listener );
  }

  public void removeProperyChangeListener( final IPropertyChangeListener listener )
  {
    getPropetyChangedListeners().remove( listener );
  }

  public void firePropertyChange( final String id, final Object oldValue, final Object newValue )
  {
    final PropertyChangeEvent event = new PropertyChangeEvent( this, id, oldValue, newValue );
    final Object[] propetyChangedListeners = getPropetyChangedListeners().getListeners();
    for( final Object element : propetyChangedListeners )
    {
      ((IPropertyChangeListener) element).propertyChange( event );

    }
  }

  public ListenerList getPropetyChangedListeners( )
  {
    if( m_listeners == null )
      m_listeners = new ListenerList();
    return m_listeners;
  }

  public void removeChild( final Object child )
  {
    if( m_filter.equals( child ) )
      m_filter = null;
    if( m_filter instanceof ComplexFilter )
    {
      final ComplexFilter root = (ComplexFilter) m_filter;
      final Operation operation = root.getOperation();
      if( operation != null )
      {
        if( operation.equals( child ) )
        {
          root.setOperation( null );
          return;
        }
        if( operation instanceof LogicalOperation )
        {
          final List<Operation> arguments = ((LogicalOperation) operation).getArguments();
          remove( arguments, child, operation );
        }
      }
    }
  }

  private void remove( final List<Operation> arguments, final Object childToRemove, final Object parent )
  {
    for( final Object element : arguments )
    {
      if( element.equals( childToRemove ) )
      {
        if( parent instanceof LogicalOperation )
        {
          final LogicalOperation parentCast = (LogicalOperation) parent;
          final List<Operation> oldArgs = parentCast.getArguments();
          oldArgs.remove( childToRemove );
          if( oldArgs.size() == 0 )
            parentCast.setArguments( null );
          return;
        }
      }
      if( element instanceof LogicalOperation )
      {
        final LogicalOperation test = (LogicalOperation) element;
        final List<Operation> newArgs = test.getArguments();
        if( newArgs != null && newArgs.size() > 0 )
          remove( newArgs, childToRemove, test );
      }
    }
  }

  public Filter getFilter( )
  {
    return m_filter;
  }

  @Override
  public Filter clone( ) throws CloneNotSupportedException
  {
    if( m_filter != null )
    {
      return m_filter.clone();
    }
    throw new CloneNotSupportedException();
  }
}
