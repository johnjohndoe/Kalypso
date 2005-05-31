/* --------------- Kalypso-Header --------------------------------------------
 
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
  
------------------------------------------------------------------------------------ */
package org.bce.eclipse.ui.views.propertysheet;

import org.bce.eclipse.jface.viewers.DefaultTableViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * SimplePropertySheetViewer
 * <p>
 * 
 * @author schlienger (24.05.2005)
 */
public class SimplePropertySheetViewer extends Viewer
{
  private DefaultTableViewer m_viewer;
  private final int m_propColSize;
  private final int m_valueColSize;

  public SimplePropertySheetViewer( final Composite parent )
  {
    this( parent, 100, 300 );
  }
  
  public SimplePropertySheetViewer( final Composite parent, final int propColSize, final int valueColSize )
  {
    m_propColSize = propColSize;
    m_valueColSize = valueColSize;
    createControl( parent );
  }

  private final void createControl( final Composite parent )
  {
    m_viewer = new DefaultTableViewer( parent, SWT.BORDER | SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL );
    m_viewer.addColumn( "property", "Eigenschaft", m_propColSize, false );
    m_viewer.addColumn( "value", "Wert", m_valueColSize, false );
    m_viewer.refreshColumnProperties();
    
    m_viewer.setLabelProvider( new PropertySheetTableLabelProvider( null ) );
    m_viewer.setContentProvider( new PropertySheetTableContentProvider() );
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  public Control getControl()
  {
    return m_viewer.getControl();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getInput()
   */
  public Object getInput()
  {
    return m_viewer.getInput();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getSelection()
   */
  public ISelection getSelection()
  {
    return m_viewer.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#refresh()
   */
  public void refresh()
  {
    m_viewer.refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setInput(java.lang.Object)
   */
  public void setInput( final Object input )
  {
    m_viewer.setInput( input );
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setSelection(org.eclipse.jface.viewers.ISelection, boolean)
   */
  public void setSelection( ISelection selection, boolean reveal )
  {
    m_viewer.setSelection( selection, reveal );
  }
}
