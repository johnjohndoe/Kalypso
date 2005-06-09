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
package org.kalypso.ogc.gml.table.celleditors;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * @author Belger
 */
public abstract class DialogCellEditor extends CellEditor
{
  private Object m_value;
/**
 * 
 */
  public DialogCellEditor()
  {
    super();
  }
/**
 * 
 */
  public DialogCellEditor( final Composite parent )
  {
    super( parent );
  }
/**
 * 
 */
  public DialogCellEditor( final Composite parent, int style )
  {
    super( parent, style );
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#createControl(org.eclipse.swt.widgets.Composite)
   */
  protected Control createControl( final Composite parent )
  {
    final Text text = new Text( parent, SWT.CENTER );
    text.setText( "<Element wird gerade editiert>" );
    text.setBackground( parent.getDisplay().getSystemColor( SWT.COLOR_DARK_GRAY ) );
    return text;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doGetValue()
   */
  protected Object doGetValue()
  {    
    return m_value;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetFocus()
   */
  protected void doSetFocus()
  {
    if( openDialog( getControl() ) )
    {
      setValueValid( true );
      fireApplyEditorValue();
    }
    else
      fireCancelEditor();
  }

  protected abstract boolean openDialog( final Control control );

  /**
   * @see org.eclipse.jface.viewers.CellEditor#doSetValue(java.lang.Object)
   */
  protected void doSetValue( final Object value )
  {
    m_value = value;
  }

  /**
   * @see org.eclipse.jface.viewers.CellEditor#activate()
   */
  public void activate()
  {
    super.activate();
  }
}
