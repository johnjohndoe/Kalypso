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
package org.kalypso.ogc.gml.om.table;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;

public class TupleResultCellModifier implements ICellModifier
{
  private final TupleResultContentProvider m_contentProvider;

  public TupleResultCellModifier( final TupleResultContentProvider contentProvider )
  {
    m_contentProvider = contentProvider;
  }

  public boolean canModify( final Object element, final String handlerId )
  {
    final IComponentUiHandler handler = m_contentProvider.getHandler( handlerId );
    return handler.isEditable();
  }

  public Object getValue( final Object element, final String handlerId )
  {
    final IRecord record = (IRecord) element;
    final IComponentUiHandler handler = m_contentProvider.getHandler( handlerId );

    return handler.getValue( record );
  }

  public void modify( final Object element, final String property, final Object value )
  {
    final TableItem item = (TableItem) element;
    final IRecord record = (IRecord) item.getData();
    modifyRecord( record, property, value );
  }

  /**
   * Does not inform any listeners.
   * <p>
   * Only changes the record if the new value is different from the current value.
   * 
   * @return the component which was modified, <code>null</code> if the record was not changed.
   */
  public void modifyRecord( final IRecord record, final String handlerId, final Object value )
  {
    final IComponentUiHandler compHandler = m_contentProvider.getHandler( handlerId );

    compHandler.setValue( record, value );
  }
}