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
package org.kalypso.eclipse.swt.custom;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ICellEditorListener;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.java.util.Arrays;

/**
 * 
 * TODO Selection besser handeln <br>
 * TODO Popupmenu auf selection <br>
 * TODO Popupmenu auf celle TODO testen
 * 
 * @author Belger
 */
public class ExcelLikeTableCursor extends TableCursor
{
  // it is difficult to debug thinks like event
  // in eclipse debugmode, so here some printouts
  // can be enabled
  boolean DEBUG = false;

  protected final TableViewer m_viewer;

  final Color m_cannotEditColor;

  final Color m_canEditColor;

  final MouseListener m_cellEditorMouseListener = new MouseAdapter()
  {
    /**
     * 
     * @see org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events.MouseEvent)
     */
    public void mouseDown( final MouseEvent e )
    {
      if( e.button == 1 )
      {
        m_tableCursorSelectionListener.widgetDefaultSelected( null );
      }
    }
  };

  final SelectionListener m_tableCursorSelectionListener = new SelectionListener()
  {
    /**
     * 
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected( SelectionEvent e )
    {
      if( DEBUG )
        System.out.println( "widgetSelected" );
      final TableItem row = getRow();
      final int widgetCol = getColumn();
      // change background color when cell is not editable
      final boolean canModify = checkCanModify( row, widgetCol );
      setBackground( canModify ? m_canEditColor : m_cannotEditColor );
      setVisible( false ); // wichtig
    }

    /**
     * 
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetDefaultSelected( final SelectionEvent e )
    {
      startEditing( null );
    }
  };

  final ICellEditorListener m_cellEditorListener = new ICellEditorListener()
  {
    // after editing set tablecursor visible
    // and give it the focus to continue navigating (e.g. CR-Up, RC-Down)
    public void applyEditorValue()
    {
      setVisible( true );
      setFocus();
      m_viewer.update( getRow().getData(), null );
      m_viewer.refresh();
      // leaf cell
    }

    public void cancelEditor()
    {
      setVisible( true );
      setFocus();
      m_viewer.update( getRow().getData(), null );
      m_viewer.refresh();
      // leaf cell
    }

    public void editorValueChanged( boolean oldValidState, boolean newValidState )
    {
    }
  };

  // keylistener while editing a cell
  final KeyListener m_keyListenerOnCell = new KeyAdapter()
  //  new KeyListener()
  {
    public void keyPressed( KeyEvent e )
    {
      // handle cursor moving
      int dx = 0, dy = 0;
      if( e.keyCode == SWT.ARROW_LEFT )
        dx = -1;
      else if( e.keyCode == SWT.ARROW_RIGHT )
        dx = 1;
      else if( e.keyCode == SWT.ARROW_UP )
        dy = -1;
      else if( e.keyCode == SWT.ARROW_DOWN )
        dy = 1;
      else if( e.keyCode == SWT.ESC )
      {
        //TODO handle esc
      }
      if( dx != 0 || dy != 0 )
      {
        final int col = getColumn() + dx;
        final Table table2 = m_viewer.getTable();
        TableItem row2 = getRow();
        final int row = table2.indexOf( row2 ) + dy;

        if( col > 0 && col < table2.getColumnCount() && row > 0 && row < table2.getItemCount() )
        {
          setSelection( row, col );
        }
        // TODO check this ???
        setVisible( true );
        setFocus();
        // leaf cell
        ( (Control)e.getSource() ).removeKeyListener( m_keyListenerOnCell );
      }
      //      if( e.keyCode == SWT.CR && e.getSource() instanceof CheckboxCellEditor
      // )
      //      {
      //        CheckboxCellEditor ce = (CheckboxCellEditor)e.getSource();
      //        ce.setValue( new Boolean( !( (Boolean)ce.getValue() ).booleanValue() )
      // );
      //      }
    }
  };

  /**
   * @see org.eclipse.swt.widgets.Control#setLocation(int, int)
   */
  public void setLocation( int x, int y )
  {
    //    TableItem row2 = getRow();
    //    // int column2 = getColumn();
    //    m_viewer.update( row2.getData(), null );
    super.setLocation( x, y );
  }

  // keylistener on table
  final KeyListener m_keyListenerOnTableCursor = new KeyAdapter()
  {

    public void keyPressed( final KeyEvent e )
    {
      //      clearSelection();
      if( e.keyCode == SWT.CTRL || ( e.stateMask & SWT.CONTROL ) != 0 )
      {
        toggleSelection( getRow() );
        //        setVisible( false );
        return;
      }
      if( e.keyCode == SWT.SHIFT || ( e.stateMask & SWT.SHIFT ) != 0 )
      {
        //        setVisible( false ); //
        //        return;
      }
      // handle F2 to start editing
      if( e.keyCode == SWT.F2
          || ( e.character == ' ' || ( e.character >= '0' && e.character <= 'z' ) || ( e.character >= 'A' && e.character <= 'Z' ) ) )
      {
        startEditing( e );
        return;
      }

      //      if( ( e.stateMask & SWT.MOD1 ) != 0 )
      //      {
      //        if( DEBUG )
      //          System.out.println( "MOD1 Alt GR" );
      //      }
      //
      //      if( ( e.stateMask & SWT.MOD2 ) != 0 )
      //      {
      //        if( DEBUG )
      //          System.out.println( "MOD2 Shift" );
      //        if( e.keyCode == SWT.ARROW_DOWN || e.keyCode == SWT.ARROW_UP ||
      // e.keyCode == SWT.ARROW_LEFT || e.keyCode == SWT.ARROW_RIGHT )
      //          setVisible( false );
      //      }
      //
      //      if( ( e.stateMask & SWT.MOD3 ) != 0 )
      //      {
      //        if( DEBUG )
      //          System.out.println( "MOD3 " );
      //      }
      //
      //      if( ( e.stateMask & SWT.MOD4 ) != 0 )
      //      {
      //        if( DEBUG )
      //          System.out.println( "MOD4 " );
      //      }
      //
      //      if( e.keyCode == SWT.MOD1 )
      //      {
      //        if( DEBUG )
      //          System.out.println( "MOD1==" );
      //      }
      //      if( e.keyCode == SWT.MOD2 )
      //      {
      //        if( DEBUG )
      //          System.out.println( "MOD2==" );
      //        int locX = m_viewer.getTable().indexOf( getRow() );
      //        int locY = getColumn();
      //        // setLocation( locX, locY );
      //        // addToSelection( getRow() );
      //        // setLocation( locX, locY );
      //        // Event event = new Event();
      //        // event.display = m_viewer.getControl().getDisplay();
      //        // event.doit = true;
      //        // event.item = getRow();
      //        // event.x = locX;
      //        // event.y = locY;
      //        // event.type = SWT.Selection;
      //        // notifyListeners( SWT.Selection, event );
      //      }
      //      // Hide the TableCursor when the user hits the "MOD1" or "MOD2" key.
      //      // This alows the user to select multiple items in the table.
      //      if( e.keyCode == SWT.MOD1 || e.keyCode == SWT.MOD2 || ( e.stateMask &
      // SWT.MOD1 ) != 0 || ( e.stateMask & SWT.MOD2 ) != 0 )
      //        setVisible( false );
      setVisible( true );
      setFocus();
    }
  };

  private final KeyListener m_tableKeyListener = new KeyAdapter()
  {
    public void keyReleased( KeyEvent e )
    {
      if( e.keyCode == SWT.CONTROL && ( e.stateMask & SWT.SHIFT ) != 0 )
        return;
      if( e.keyCode == SWT.SHIFT && ( e.stateMask & SWT.CONTROL ) != 0 )
        return;
      if( e.keyCode != SWT.CONTROL && ( e.stateMask & SWT.CONTROL ) != 0 )
        return;
      if( e.keyCode != SWT.SHIFT && ( e.stateMask & SWT.SHIFT ) != 0 )
        return;
      setVisible( true ); // wichtig
      setFocus();// wichtig

      //      final Table table = m_viewer.getTable();
      //      TableItem[] selection = table.getSelection();
      //      TableItem row = ( selection.length == 0 ) ? table.getItem(
      // table.getTopIndex() ) : selection[0];
      //      table.showItem( getRow() );
    }
  };

  public ExcelLikeTableCursor( final TableViewer tableViewer, final int style )
  {
    super( tableViewer.getTable(), style );
    m_viewer = tableViewer;
    m_cannotEditColor = getDisplay().getSystemColor( SWT.COLOR_GRAY );
    m_canEditColor = getBackground();

    // add keylistener to start editing on key pressed
    addKeyListener( m_keyListenerOnTableCursor );

    // change background color when cell is not editable
    addSelectionListener( m_tableCursorSelectionListener );

    addFocusListener( new FocusListener()
    {
      // cursor gets focus == editing stoped
      public void focusGained( FocusEvent e )
      {
        //        System.out.println( "focusGained" );
      }

      // cursor loose focus == editing started
      public void focusLost( FocusEvent e )
      {
        //        System.out.println( "focusLost" );
      }
    } );
    // Show the TableCursor when the user releases the "MOD2" or "MOD1" key.
    // This signals the end of the multiple selection task.
    final Table table = tableViewer.getTable();
    table.addKeyListener( m_tableKeyListener );
    addMouseListener( m_cellEditorMouseListener );
  }

  void startEditing( final KeyEvent keyEvent )
  {
    final int column = getColumn();
    final TableItem tableRow = getRow();
    if( tableRow == null )
      return;

    // get current value of the cell
    final Object element = tableRow.getData();

    // do nothing if cell is not editable
    if( !checkCanModify( tableRow, column ) )
      return;

    // tablecursor should be invisible while editing the cell
    setVisible( false );

    // add the editorListener to the celleditor in order to refocus the
    // tablecursor
    final CellEditor cellEditor = m_viewer.getCellEditors()[column];

    cellEditor.addListener( m_cellEditorListener );
    // TODO check remove listener

    // control arrow and ESC keys
    // TODO check how listener will be removed if no key gets pressed
    // remove potential old listener
    final Control control = cellEditor.getControl();
    if( control != null && !control.isDisposed() )
      control.removeKeyListener( m_keyListenerOnCell );
    if( keyEvent != null && keyEvent.keyCode != SWT.F2 && cellEditor instanceof TextCellEditor )
      control.addKeyListener( m_keyListenerOnCell );
    if( keyEvent != null && keyEvent.keyCode != SWT.F2 && cellEditor instanceof CheckboxCellEditor )
      control.addKeyListener( m_keyListenerOnCell );
    m_viewer.editElement( element, column );
    final Widget editorControl = control;

    // eigentlich würde ich gerne direkt den event weiterschicken, das klappt
    // aber nicht
    // ??
    //    final Event event = new Event();
    //    event.type = SWT.KeyDown;
    //    event.character = keyEvent.character;
    //    event.keyCode = keyEvent.keyCode;
    //    // wäre schön, jetzt ein KeyPressed abzusetzen
    //    editorControl.notifyListeners( SWT.KeyDown, event );
    // ??
    // do not loose pressed character
    if( editorControl instanceof Button )
    {
      final Button button = (Button)editorControl;
      if( keyEvent != null )
        button.setSelection( !button.getSelection() );
    }
    if( editorControl instanceof Text )
    {
      final Text text = (Text)editorControl;
      if( keyEvent != null && keyEvent.keyCode != SWT.F2 )
        text.insert( "" + keyEvent.character );
    }
  }

  boolean checkCanModify( final TableItem row, final int column )
  {
    if( m_viewer == null )
      return false;

    final String property = m_viewer.getColumnProperties()[column].toString();
    final ICellModifier modifier = m_viewer.getCellModifier();
    if( modifier == null )
      return false;

    return modifier.canModify( row.getData(), property );
  }

  void addToSelection( TableItem item )
  {
    if( DEBUG )
      System.out.println( "add to selection" );
    final IStructuredSelection tableSelection = (IStructuredSelection)m_viewer.getSelection();
    final List featureList = new ArrayList( tableSelection.toList() );
    featureList.add( item.getData() );
    final IStructuredSelection newSelection = new StructuredSelection( featureList );

    final TableItem[] viewerSelection = m_viewer.getTable().getSelection();
    final List itemList = Arrays.asList( viewerSelection );
    itemList.add( item );
    final TableItem[] newViewerSelection = (TableItem[])itemList.toArray( new TableItem[itemList.size()] );
    m_viewer.setSelection( newSelection, true );
    m_viewer.getTable().setSelection( newViewerSelection );
  }

  void removeFromSelection( TableItem item )
  {
    if( DEBUG )
      System.out.println( "remove selection" );
    final IStructuredSelection tableSelection = (IStructuredSelection)m_viewer.getSelection();
    final List featureList = tableSelection.toList();
    featureList.remove( item.getData() );
    final IStructuredSelection newSelection = new StructuredSelection( featureList );

    final TableItem[] viewerSelection = m_viewer.getTable().getSelection();
    final List itemList = Arrays.asList( viewerSelection );
    itemList.remove( item );
    final TableItem[] newViewerSelection = (TableItem[])itemList.toArray( new TableItem[itemList.size()] );
    m_viewer.setSelection( newSelection, true );
    m_viewer.getTable().setSelection( newViewerSelection );
  }

  void toggleSelection( TableItem item )
  {
    if( DEBUG )
      System.out.println( "toggle selection" );
    final IStructuredSelection tableSelection = (IStructuredSelection)m_viewer.getSelection();
    final List featureList = new ArrayList( tableSelection.toList() );
    final Object feature = item.getData();
    if( featureList.contains( feature ) )
      featureList.remove( feature );
    else
      featureList.add( feature );
    final IStructuredSelection newSelection = new StructuredSelection( featureList );
    final TableItem[] viewerSelection = m_viewer.getTable().getSelection();
    final List itemList = Arrays.asList( viewerSelection );
    if( itemList.contains( item ) )
      itemList.remove( item );
    else
      itemList.add( item );
    final TableItem[] newViewerSelection = (TableItem[])itemList.toArray( new TableItem[itemList.size()] );
    m_viewer.setSelection( newSelection, true );
    m_viewer.getTable().setSelection( newViewerSelection );
  }

  void clearSelection()
  {
    if( DEBUG )
      System.out.println( "clear selection" );
    final ISelection selection = m_viewer.getSelection();
    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      // clear selections
      m_viewer.setSelection( null );
      m_viewer.getTable().setSelection( new TableItem[0] );
      // refresh view
      final List list = ( (IStructuredSelection)selection ).toList();
      for( Iterator iter = list.iterator(); iter.hasNext(); )
      {
        Object element = iter.next();
        m_viewer.update( element, null );
      }
    }
  }
}