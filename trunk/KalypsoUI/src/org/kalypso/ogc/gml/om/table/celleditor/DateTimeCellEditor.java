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
package org.kalypso.ogc.gml.om.table.celleditor;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Date Time Cell Editor for XMLGregorianCalendar Observation Components
 * 
 * @author Dirk Kuch
 */
public class DateTimeCellEditor extends CellEditor
{
  private DateTimeDialog m_dialog;

  public DateTimeCellEditor( final Composite parent )
  {
    super( parent );
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createControl( final Composite parent )
  {
    m_dialog = new DateTimeDialog( parent.getShell() );

    return m_dialog.getShell();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#activate()
   */
  @Override
  public void activate( )
  {
    if( m_dialog.open() == Window.OK )
    {
      markDirty();

      fireApplyEditorValue();
    }
    else
      fireCancelEditor();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
   */
  @Override
  protected Object doGetValue( )
  {
    return new XMLGregorianCalendarImpl( m_dialog.getDateTime() );
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetFocus()
   */
  @Override
  protected void doSetFocus( )
  {
    // FIXME
    m_dialog.getShell().setFocus();
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
   */
  @Override
  protected void doSetValue( final Object value )
  {
    if( value instanceof XMLGregorianCalendar )
    {
      final XMLGregorianCalendar calendar = (XMLGregorianCalendar) value;
      m_dialog.setDateTime( calendar.toGregorianCalendar() );
    }
    else
      throw new IllegalStateException();
  }

}
