package org.kalypso.eclipse.swt.custom;

import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.PlatformUI;

/**
 * 
 * @author Belger
 */
public class ExcelLikeTableCursor extends TableCursor implements SelectionListener, KeyListener,
    DisposeListener, MouseListener
{
  protected final TableViewer m_viewer;

  /**
   * Hack, um das Element richtig anzuzeigen nach dem edit! Problem ist, dass
   * die Aktualisierung der tabelle schnell genug sein muss
   */
  private final TimerTask m_timertask = new TimerTask()
  {
    public void run()
    {
      PlatformUI.getWorkbench().getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          if( !isDisposed() && !isVisible() && !m_viewer.isCellEditorActive() )
          {
            try
            {
              Thread.sleep( 100 );
            }
            catch( InterruptedException e )
            {
              e.printStackTrace();
            }
            stopEditing();
          }
        }
      } );
    }
  };

  private final Color m_cannotEditColor;
  private final Color m_canEditColor;

  public ExcelLikeTableCursor( final TableViewer tableViewer, final int style )
  {
    super( tableViewer.getTable(), style );

    m_viewer = tableViewer;

    addSelectionListener( this );
    addKeyListener( this );
    addMouseListener( this );

    new Timer().schedule( m_timertask, 100, 100 );

    addDisposeListener( this );
    
    m_cannotEditColor = getDisplay().getSystemColor( SWT.COLOR_GRAY );
    
    m_canEditColor = getBackground();
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    m_timertask.cancel();
    
    super.dispose();
  }

  private void startEditing( final KeyEvent ke )
  {
    final int column = getColumn();
    final TableItem tableRow = getRow();
    final Object element = tableRow.getData();
    
    if( !checkCanModify( tableRow, column ) )
      return;

    setVisible( false );

    m_viewer.editElement( element, column );
    if( ke != null )
    {
      final Widget editorControl = m_viewer.getCellEditors()[column].getControl();

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

      // deshalb einfach den text setzen
      if( editorControl instanceof Text )
        ( (Text)editorControl ).insert( "" + ke.character );
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
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
  // ignore
  }

  /**
   * @see org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt.events.DisposeEvent)
   */
  public void widgetDisposed( final DisposeEvent e )
  {
    m_timertask.cancel();
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