package org.kalypso.ogc.sensor.view;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

/**
 * @author schlienger
 *
 */
public class TableViewPart extends ViewPart implements ISelectionChangedListener
{
  private TableViewer m_viewer = null;
  
  public TableViewPart()
  
  {
    // noch nix
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    m_viewer = new TableViewer( parent, SWT.MULTI );
    
    
    m_viewer.getTable().setHeaderVisible( true );
    m_viewer.getTable().setLinesVisible( true );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    // noch nix
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    // copy from DiagramViewPart...
  }
}
