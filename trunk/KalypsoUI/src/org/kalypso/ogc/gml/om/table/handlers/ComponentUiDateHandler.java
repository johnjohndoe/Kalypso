/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.Calendar;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.celleditor.DateTimeCellEditor;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Handles XMLGreogorianCalendar types.
 * 
 * @author Dirk Kuch
 */
public class ComponentUiDateHandler extends AbstractComponentUiHandler
{
  public ComponentUiDateHandler( final int component, final boolean editable, final boolean resizeable, final boolean moveable, final String columnLabel, final int columnStyle, final int columnWidth, final int columnWidthPercent, final String displayFormat, final String nullFormat, final String parseFormat )
  {
    super( component, editable, resizeable, moveable, columnLabel, columnStyle, columnWidth, columnWidthPercent, displayFormat, nullFormat, parseFormat );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#createCellEditor(org.eclipse.swt.widgets.Table)
   */
  public CellEditor createCellEditor( final Table table )
  {
    return new DateTimeCellEditor( table );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#formatValue(org.kalypso.observation.result.IRecord)
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
    record.setValue( getComponent(), value );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getStringRepresentation(java.lang.Object)
   */
  @Override
  public String getStringRepresentation( final IRecord record )
  {
    final int component = getComponent();

    final Object value = record.getValue( component );

    if( value instanceof XMLGregorianCalendar )
    {
      final XMLGregorianCalendar xmlCal = (XMLGregorianCalendar) value;
      final Date date = DateUtilities.toDate( xmlCal );

      if( date == null )
        return String.format( getNullFormat() );

      final Calendar instance = Calendar.getInstance( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
      instance.setTime( date );

      final String displayFormat = getDisplayFormat();

      return String.format( displayFormat, instance, instance, instance );
    }

    return "<No Date>";
  }
}
