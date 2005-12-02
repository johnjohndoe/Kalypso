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
package org.kalypso.contribs.eclipse.swt.custom;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

/**
 * 
 * @author Belger
 */
public class OldExcelLikeTableCursor extends TableCursor implements SelectionListener, KeyListener, MouseListener
{
  protected final TableViewer m_viewer;

  private final Color m_cannotEditColor;

  private final Color m_canEditColor;

  private final ICellEditorListener m_editorListener = new ICellEditorListener()
  {
    public void applyEditorValue()
    {
      stopEditing();
    }

    public void cancelEditor()
    {
      stopEditing();
    }

    public void editorValueChanged( boolean oldValidState, boolean newValidState )
    {
    // nix
    }
  };

  public OldExcelLikeTableCursor( final TableViewer tableViewer, final int style )
  {
    super( tableViewer.getTable(), style );

    m_viewer = tableViewer;

    addSelectionListener( this );
    addKeyListener( this );
    addMouseListener( this );

    m_cannotEditColor = getDisplay().getSystemColor( SWT.COLOR_GRAY );

    m_canEditColor = getBackground();

    // Show the TableCursor when the user releases the "MOD2" or "MOD1" key.
    // This signals the end of the multiple selection task.
    final Table table = tableViewer.getTable();
    table.addKeyListener( new KeyAdapter()
    {
      public void keyReleased( KeyEvent e )
      {
        if( e.keyCode == SWT.MOD1 && ( e.stateMask & SWT.MOD2 ) != 0 )
          return;
        if( e.keyCode == SWT.MOD2 && ( e.stateMask & SWT.MOD1 ) != 0 )
          return;
        if( e.keyCode != SWT.MOD1 && ( e.stateMask & SWT.MOD1 ) != 0 )
          return;
        if( e.keyCode != SWT.MOD2 && ( e.stateMask & SWT.MOD2 ) != 0 )
          return;

        setVisible( true );
        setFocus();
      }
    } );
  }

  private void startEditing( final KeyEvent ke )
  {
    final int column = getColumn();
    final TableItem tableRow = getRow();
    if( tableRow == null )
      return;

    final Object element = tableRow.getData();

    if( !checkCanModify( tableRow, column ) )
      return;

    setVisible( false );

    final CellEditor cellEditor = m_viewer.getCellEditors()[column];
    cellEditor.addListener( m_editorListener );

    m_viewer.editElement( element, column );

    if( ke != null )
    {
      final Widget editorControl = cellEditor.getControl();

      // eigentlich würde ich gerne direkt den event weiterschicken, das klappt
      // aber nicht
      //        final Event event = new Event();
      //        event.type = SWT.KeyDown;
      //        event.character = ke.character;
      //        event.keyCode = ke.keyCode;
      //        
      //        
      //        // wäre schön, jetzt ein KeyPressed abzusetzen
      //        editorControl.notifyListeners( SWT.KeyDown, event );

      // stattdessen einfach den text setzen
      if( editorControl instanceof Text )
      {
        final Text text = (Text)editorControl;

        //text.setText( "" + ke.character );

        text.insert( "" + ke.character );
      }
    }

  }

  public void stopEditing()
  {
    setVisible( true );
    setFocus();
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    // change color oder so, wenn Zelle nicht editierbar
    final TableItem row = getRow();
    final int column = getColumn();

    final boolean canModify = checkCanModify( row, column );

    setBackground( canModify ? m_canEditColor : m_cannotEditColor );
  }

  private boolean checkCanModify( final TableItem row, final int column )
  {
    if( m_viewer == null )
      return false;

    final String property = m_viewer.getColumnProperties()[column].toString();
    final ICellModifier modifier = m_viewer.getCellModifier();
    if( modifier == null )
      return false;

    return modifier.canModify( row.getData(), property );
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( final SelectionEvent e )
  {
    startEditing( null );
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    if( e.keyCode == SWT.F2 )
      startEditing( null );
    else if( e.character >= '0' && e.character <= 'z' )
      startEditing( e );

    // Hide the TableCursor when the user hits the "MOD1" or "MOD2" key.
    // This alows the user to select multiple items in the table.
    if( e.keyCode == SWT.MOD1 || e.keyCode == SWT.MOD2 || ( e.stateMask & SWT.MOD1 ) != 0
        || ( e.stateMask & SWT.MOD2 ) != 0 )
      setVisible( false );
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
  // ignore
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseDoubleClick( final MouseEvent e )
  {
  // nichts tun
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseDown( final MouseEvent e )
  {
    startEditing( null );
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseUp( MouseEvent e )
  {
  // nix tun
  }
}