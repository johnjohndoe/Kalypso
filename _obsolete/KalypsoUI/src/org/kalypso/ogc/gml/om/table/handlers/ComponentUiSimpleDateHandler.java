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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.i18n.Messages;
import org.kalypso.observation.result.IRecord;

/**
 * @author burtscher1 A handler which allows editing of dates as strings
 */
public class ComponentUiSimpleDateHandler extends AbstractComponentUiHandler
{

  private final SimpleDateFormat m_df;

  public ComponentUiSimpleDateHandler( int component, boolean editable, boolean resizeable, boolean moveable, String columnLabel, int columnStyle, int columnWidth, int columnWidthPercent, String displayFormat, String nullFormat, String parseFormat )
  {
    super( component, editable, resizeable, moveable, columnLabel, columnStyle, columnWidth, columnWidthPercent, displayFormat, nullFormat, parseFormat );
    m_df = new SimpleDateFormat( "dd.MM.yyyy hh:mm" );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#createCellEditor(org.eclipse.swt.widgets.Table)
   */
  @Override
  public CellEditor createCellEditor( Table table )
  {
    return new TextCellEditor( table, SWT.NONE );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#doGetValue(org.kalypso.observation.result.IRecord)
   */
  @Override
  public Object doGetValue( IRecord record )
  {
    final Object value = record.getValue( getComponent() );
    if( value == null )
      return ""; //$NON-NLS-1$

    return getStringRepresentation( record );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#doSetValue(org.kalypso.observation.result.IRecord,
   *      java.lang.Object)
   */
  @Override
  public void doSetValue( IRecord record, Object value )
  {
    if( (value == null) || (value.toString().trim().length() == 0) )
      setValue( record, null );
    else
      setValue( record, parseValue( value.toString() ) );

  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#parseValue(java.lang.String)
   */
  @Override
  public Object parseValue( String text )
  {
    try
    {
      Date date = m_df.parse( text );
      return DateUtilities.toXMLGregorianCalendar( date );
    }
    catch( ParseException e )
    {
      throw new IllegalArgumentException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#setValue(org.kalypso.observation.result.IRecord,
   *      java.lang.Object)
   */
  @Override
  public void setValue( IRecord record, Object value )
  {
    record.setValue( getComponent(), value );
  }

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

// final Calendar instance = Calendar.getInstance( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
      final Calendar instance = Calendar.getInstance();
      instance.setTime( date );

      final String displayFormat = getDisplayFormat();

      return String.format( displayFormat, instance, instance, instance );
    }

    return Messages.getString( "org.kalypso.ogc.gml.om.table.handlers.ComponentUiDateHandler.0" ); //$NON-NLS-1$
  }

}
