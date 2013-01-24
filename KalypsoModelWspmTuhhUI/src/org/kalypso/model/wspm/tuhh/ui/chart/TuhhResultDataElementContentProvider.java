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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultContentProvider;

/**
 * @author Gernot Belger
 */
public class TuhhResultDataElementContentProvider implements ITreeContentProvider
{
  private final ITreeContentProvider m_delegate = new WspmResultContentProvider();

  /**
   * @param inputElement
   * @return
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object inputElement )
  {
    final Object[] elements = m_delegate.getElements( inputElement );
    final Object[] wrappedElements = wrapElements( elements );
    if( wrappedElements.length == 0 )
      return new TuhhResultDataElement[] { new TuhhResultDataElement( null ) };

    return wrappedElements;
  }

  private Object[] wrapElements( final Object[] elements )
  {
    final TuhhResultDataElement[] wrappedElements = new TuhhResultDataElement[elements.length];
    for( int i = 0; i < wrappedElements.length; i++ )
      wrappedElements[i] = new TuhhResultDataElement( (IWspmResultNode) elements[i] );

    return wrappedElements;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    m_delegate.dispose();
  }

  /**
   * @param viewer
   * @param oldInput
   * @param newInput
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_delegate.inputChanged( viewer, oldInput, newInput );
  }

  /**
   * @param parentElement
   * @return
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    final IWspmResultNode parentNode = asResultNode( parentElement );
    if( parentNode == null )
      return ArrayUtils.EMPTY_OBJECT_ARRAY;

    final Object[] children = m_delegate.getChildren( parentNode );
    return wrapElements( children );
  }

  /**
   * @param element
   * @return
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element )
  {
    final IWspmResultNode resultNode = asResultNode( element );
    if( resultNode == null )
      return null;

    final Object parent = m_delegate.getParent( resultNode );
    return new TuhhResultDataElement( (IWspmResultNode) parent );
  }

  public static IWspmResultNode asResultNode( final Object element )
  {
    final TuhhResultDataElement resultElement = (TuhhResultDataElement) element;
    return resultElement.getResultNode();
  }

  /**
   * @param element
   * @return
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    final IWspmResultNode resultNode = asResultNode( element );
    if( resultNode == null )
      return false;

    return m_delegate.hasChildren( resultNode );
  }

}
