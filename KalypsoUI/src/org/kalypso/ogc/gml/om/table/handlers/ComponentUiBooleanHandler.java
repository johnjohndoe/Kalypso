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
package org.kalypso.ogc.gml.om.table.handlers;

import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Table;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.celleditor.ComboBoxViewerCellEditor;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * Handles enumerated values, i.e. values which have enumeration-restrictions.
 * 
 * @author Dirk Kuch
 * @author Gernot Belger
 */
public class ComponentUiBooleanHandler extends AbstractComponentUiHandler
{

  public ComponentUiBooleanHandler( final IComponent component, final boolean editable, final boolean resizeable, final boolean moveable, final String columnLabel, final int columnStyle, final int columnWidth, final int columnWidthPercent, final String displayFormat, final String nullFormat, final String parseFormat )
  {
    super( component, editable, resizeable, moveable, columnLabel, columnStyle, columnWidth, columnWidthPercent, displayFormat, nullFormat, parseFormat );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#createCellEditor(org.eclipse.swt.widgets.Table)
   */
  public CellEditor createCellEditor( final Table table )
  {
    final Set<Boolean> set = new LinkedHashSet<Boolean>();
    set.add( true );
    set.add( false );

    final LabelProvider labelProvider = new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        if( element instanceof Boolean )
        {
          Boolean b = (Boolean) element;

          if( b == true )
            return "true";
          else if( b == false )
            return "false";
        }

        return element.toString();
      }
    };

    return new ComboBoxViewerCellEditor( new ArrayContentProvider(), labelProvider, set, table, SWT.READ_ONLY | SWT.DROP_DOWN );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#formatValue(java.lang.Object)
   */
  public Object getValue( final IRecord record )
  {
    return record.getValue( getComponent() );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#setValue(org.kalypso.observation.result.IRecord,
   *      java.lang.Object)
   */
  public void setValue( final IRecord record, final Object value )
  {
    if( value == null )
      record.setValue( getComponent(), false );
    else
      record.setValue( getComponent(), value );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getStringRepresentation(org.kalypso.observation.result.IRecord)
   */
  @Override
  public String getStringRepresentation( final IRecord record )
  {
    if( getComponent() == null )
      throw new UnsupportedOperationException( "No compoentn specified, overwrite this method." );

    final Object value = record.getValue( getComponent() );

    if( value instanceof Boolean )
    {
      final Boolean b = (Boolean) value;

      if( b == true )
        return "true";
      else if( b == false )
        return "false";
    }

    throw new NotImplementedException();
  }
}