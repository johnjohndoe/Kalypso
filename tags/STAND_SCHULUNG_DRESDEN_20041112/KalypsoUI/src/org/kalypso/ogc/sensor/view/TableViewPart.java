package org.kalypso.ogc.sensor.view;

import java.awt.Frame;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.impl.ObservationTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Table QuickView.
 * 
 * @author schlienger
 */
public class TableViewPart extends ViewPart implements
    ISelectionChangedListener, IPartListener
{
  protected final ObservationTableViewTemplate m_template = new ObservationTableViewTemplate();

  private ObservationTable m_table;

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    m_table = new ObservationTable( new ObservationTableModel() );
    m_template.addTemplateEventListener( m_table );

    // SWT-AWT Brücke für die Darstellung von JTable
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    m_table.setVisible( true );

    final JScrollPane pane = new JScrollPane( m_table );
    pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );

    getSite().getPage().addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose( )
  {
    getSite().getPage().removePartListener( this );

    m_template.removeTemplateEventListener( m_table );

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus( )
  {
    // noch nix
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
//    final Runnable runnable = new Runnable()
//    {
//      public void run( )
//      {
        m_template.removeAllThemes();

        final StructuredSelection selection = (StructuredSelection) event
            .getSelection();

        if( !(selection.getFirstElement() instanceof IRepositoryItem) )
          return;

        final IRepositoryItem item = (IRepositoryItem) selection
            .getFirstElement();

        final IObservation obs = ObservationCache.getInstance().getObservationFor( item );
        if( obs != null )
          m_template.setObservation( obs, false, ObservationViewHelper.makeDateRange( item ) );
//      }
//    };
//
//    try
//    {
//      // execute this in the swing ui thread because we are using a swing component (JTable)
//      SwingUtilities.invokeLater( runnable );
//    }
//    catch( Exception e ) // generic exception caught for simplicity
//    {
//      e.printStackTrace();
//    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partActivated( IWorkbenchPart part )
  {
    if( part != null && part instanceof RepositoryExplorerPart )
      ((RepositoryExplorerPart) part).addSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partBroughtToTop(org.eclipse.ui.IWorkbenchPart)
   */
  public void partBroughtToTop( IWorkbenchPart part )
  {
    // nada
  }

  /**
   * @see org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
   */
  public void partClosed( IWorkbenchPart part )
  {
    if( part != null && part instanceof RepositoryExplorerPart )
      ((RepositoryExplorerPart) part).removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partDeactivated( IWorkbenchPart part )
  {
    if( part != null && part instanceof RepositoryExplorerPart )
      ((RepositoryExplorerPart) part).removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
   */
  public void partOpened( IWorkbenchPart part )
  {
    // Siehe partActivated...
  }
}