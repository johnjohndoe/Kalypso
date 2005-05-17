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
package org.bce.eclipse.swt.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Widget;

/**
 * Provides fake tooltips for control's which doesnt have one. For example TableItem or Tablecolumns in Tables.
 * 
 * @author belger
 */
public abstract class AbstractControlTooltipListener implements Listener
{
  protected static final void hookListener( final Widget widget, final AbstractControlTooltipListener tableListener )
  {
    widget.addListener( SWT.Dispose, tableListener );
    widget.addListener( SWT.KeyDown, tableListener );
    widget.addListener( SWT.MouseMove, tableListener );
    widget.addListener( SWT.MouseHover, tableListener );
  }

  private Shell m_tip = null;

  private Label m_label = null;

  private final Listener m_labelListener;

  private final String m_tooltipProperty;

  private final Shell m_shell;

  public AbstractControlTooltipListener( final Shell shell, final String tooltipProperty )
  {
    m_shell = shell;
    m_tooltipProperty = tooltipProperty;

    m_labelListener = new Listener()
    {
      public void handleEvent( final Event event )
      {
        final Label label = (Label)event.widget;
        final Shell labelShell = label.getShell();
        switch( event.type )
        {
//        case SWT.MouseDown:
//          final Event e = new Event();
//          e.item = (TableItem)label.getData( "_TABLEITEM" );
//
//          // Assuming table is single select, set the selection as if
//          // the mouse down event went through to the table
//          table.setSelection( new TableItem[]
//          { (TableItem)e.item } );
//          table.notifyListeners( SWT.Selection, e );

        // fall through
        case SWT.MouseExit:
          labelShell.dispose();
          break;
        }
      }
    };
  }

  /**
   * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
   */
  public void handleEvent( final Event event )
  {
    switch( event.type )
    {
    case SWT.Dispose:
    case SWT.KeyDown:
    case SWT.MouseMove:
    {
      if( m_tip != null )
      {
        m_tip.dispose();
        m_tip = null;
        m_label = null;
      }
      break;
    }

    case SWT.MouseHover:
    {
      final Widget item = getItemForEvent( event );
      if( item != null )
      {
        if( m_tip != null && !m_tip.isDisposed() )
        {
          m_tip.dispose();
          m_tip = null;
        }

        final Object tooltipData = item.getData( m_tooltipProperty );
        final String tooltip = tooltipData == null ? null : tooltipData.toString();

        if( tooltip != null )
        {
          m_tip = new Shell( m_shell, SWT.ON_TOP | SWT.TOOL );
          m_tip.setLayout( new FillLayout() );
          m_label = new Label( m_tip, SWT.NONE );
          final Display display = m_shell.getDisplay();
          m_label.setForeground( display.getSystemColor( SWT.COLOR_INFO_FOREGROUND ) );
          m_label.setBackground( display.getSystemColor( SWT.COLOR_INFO_BACKGROUND ) );
          m_label.setData( "_TABLEITEM", item );
          m_label.setText( tooltip );
          m_label.addListener( SWT.MouseExit, m_labelListener );
          m_label.addListener( SWT.MouseDown, m_labelListener );
          final Point size = m_tip.computeSize( SWT.DEFAULT, SWT.DEFAULT );
          
//          final Rectangle rect = item.getBounds( 0 );
          
//          final Point pt = m_control.toDisplay( rect.x, rect.y );
          final Point pt = m_shell.toDisplay( event.x, event.y );
          m_tip.setBounds( pt.x, pt.y, size.x, size.y );
          m_tip.setVisible( true );
        }
      }
    }
    }
  }

  protected abstract Widget getItemForEvent( final Event event );
}
