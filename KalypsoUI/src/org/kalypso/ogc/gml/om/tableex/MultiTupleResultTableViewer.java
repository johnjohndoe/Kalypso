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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.kalypso.commons.tuple.event.ITupleModelListener;
import org.kalypso.commons.tuple.event.TupleModelAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.observation.table.MTRMColumn;
import org.kalypso.observation.table.MTRMRow;

/**
 * @author schlienger
 */
public class MultiTupleResultTableViewer extends DefaultTableViewer implements ITupleModelListener<MTRMRow, MTRMColumn>
{
  private TupleModelAdapter<MTRMRow, MTRMColumn> m_model = null;

  public MultiTupleResultTableViewer( final Composite parent )
  {
    this( parent, SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION );
  }

  public MultiTupleResultTableViewer( final Composite parent, final int style )
  {
    super( parent, style );
    
    final Table table = getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );
    
    setContentProvider( new MultiTupleResultContentProvider() );
    setLabelProvider( new MultiTupleResultLabelProvider() );
  }

  public void setInput( final TupleModelAdapter<MTRMRow, MTRMColumn> model )
  {
    // disconnect listener before changing model
    if( m_model != null && m_model != model )
      m_model.removeListener( this );
    
    m_model = model;
    m_model.addListener( this );
    
    removeAllColumns();

    for( MTRMColumn col: model.getColumnKeySet() )
      addColumn( col.getKeyName(), col.toString(), 100, true );
    
    refreshColumnProperties();
    
    ((MultiTupleResultLabelProvider) getLabelProvider()).setModel( m_model );
    
    super.setInput( model );
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onValueChanged(java.lang.Object, R, C)
   */
  public void onValueChanged( Object value, MTRMRow rowKey, MTRMColumn columnKey )
  {
    refresh();
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onRowRemoved(R)
   */
  public void onRowRemoved( MTRMRow rowKey )
  {
    refresh();
  }

  /**
   * @see org.kalypso.commons.tuple.event.ITupleModelListener#onColumnRemoved(C)
   */
  public void onColumnRemoved( MTRMColumn columnKey )
  {
    refresh();
  }
}
