package org.kalypso.eclipse.swt.custom;

import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TableCursor;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.PlatformUI;

/**
 * 
 * 
 * @author gernot
 */
public class ExcelLikeTableCursor extends TableCursor implements SelectionListener, KeyListener, DisposeListener
{
  protected final TableViewer m_viewer;

  /** Hack, um das element richtig anzuzeigen nach dem edit!
   * Problem ist, dass die aktualisierung der tabelle schnell genug sein muss 
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

  public ExcelLikeTableCursor( final TableViewer tableViewer, final int style )
  {
    super( tableViewer.getTable(), style );

    m_viewer = tableViewer;

    addSelectionListener( this );
    addKeyListener( this );

    new Timer().schedule( m_timertask, 100, 100 );
    
    addDisposeListener( this );
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    m_timertask.cancel();

    super.dispose();
  }

  private void startEditing()
  {
    final IStructuredSelection selection = (IStructuredSelection)m_viewer.getSelection();
    if( !selection.isEmpty() )
    {
      final int column = getColumn();

      setVisible( false );

      m_viewer.editElement( selection.getFirstElement(), column );

   //   m_viewer.getCellEditors()[column].addListener( this );
    }
  }

  protected void stopEditing()
  {
    final int column = getColumn();

    setVisible( true );
    setFocus();
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    m_viewer.getTable().setSelection( new TableItem[]
    { getRow() } );
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( final SelectionEvent e )
  {
    startEditing();
  }

  /**
   * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    if( e.keyCode == SWT.F2 )
      startEditing();
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
}