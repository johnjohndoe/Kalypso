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
package org.kalypso.ogc.gml.om.tableex;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.commons.tuple.ITupleModel;
import org.kalypso.commons.tuple.event.ITupleModelListener;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.observation.table.MTRMColumn;
import org.kalypso.observation.table.MTRMRow;

/**
 * @author schlienger
 */
public class MultiTupleResultContentProvider implements IStructuredContentProvider, ITupleModelListener<MTRMRow, MTRMColumn>
{
  private ITupleModel<MTRMRow, MTRMColumn> m_model;

  private DefaultTableViewer m_tableViewer;

  private final MultiTupleResultLabelProvider m_labelProvider;

  public MultiTupleResultContentProvider( final MultiTupleResultLabelProvider labelProvider )
  {
    m_labelProvider = labelProvider;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    if( m_model != null )
      m_model.removeListener( this );
    m_model = null;
    m_tableViewer = null;

    m_labelProvider.setModel( null );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_tableViewer = ((DefaultTableViewer) viewer);
    if( oldInput != null )
      m_tableViewer.removeAllColumns();

    if( newInput == null )
      return;

    // disconnect listener before changing model
    if( m_model != null && m_model == oldInput )
      m_model.removeListener( this );

    m_model = (ITupleModel<MTRMRow, MTRMColumn>) newInput;
    m_model.addListener( this );

    m_labelProvider.setModel( m_model );

    for( final MTRMColumn col : m_model.getColumnKeySet() )
      m_tableViewer.addColumn( col.getKeyName(), col.toString(), 100, -1, true );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    return m_model.getRowKeySet().toArray();
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onValueChanged(java.lang.Object, R, C)
   */
  public void onValueChanged( final Object value, final MTRMRow rowKey, final MTRMColumn columnKey )
  {
    m_tableViewer.update( m_model, null );
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onRowAdded(R)
   */
  public void onRowAdded( final MTRMRow rowKey )
  {
    m_tableViewer.refresh();
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onRowRemoved(R)
   */
  public void onRowRemoved( final MTRMRow rowKey )
  {
    m_tableViewer.refresh();
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onColumnAdded(C)
   */
  public void onColumnAdded( final MTRMColumn col )
  {
    m_tableViewer.addColumn( col.getKeyName(), col.toString(), 100, -1, true );
    m_tableViewer.refresh();
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onColumnRemoved(C)
   */
  public void onColumnRemoved( final MTRMColumn columnKey )
  {
    m_tableViewer.refresh();
  }
}
