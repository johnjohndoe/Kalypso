package org.kalypso.ogc.sensor.view;

import java.awt.Frame;
import java.util.Calendar;
import java.util.Date;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;

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
import org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.util.adapter.IAdaptable;
import org.kalypso.util.repository.view.RepositoryExplorerPart;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * Table QuickView.
 * 
 * @author schlienger
 */
public class TableViewPart extends ViewPart implements ISelectionChangedListener, IPartListener
{
  protected final ObservationTableModel m_model = new ObservationTableModel();

  private final DefaultTableViewTemplate m_template = new DefaultTableViewTemplate();

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    final ObservationTable table = new ObservationTable( m_model );
    m_template.addTemplateEventListener( m_model );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    table.setVisible( true );

    final JScrollPane pane = new JScrollPane( table );
    pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );

    getSite().getPage().addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    getSite().getPage().removePartListener( this );

    m_template.removeTemplateEventListener( m_model );
    
    super.dispose();
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
    m_template.removeAllColumns();

    StructuredSelection selection = (StructuredSelection)event.getSelection();

    if( !( selection.getFirstElement() instanceof IAdaptable ) )
      return;

    IObservation obs = (IObservation)( (IAdaptable)selection.getFirstElement() )
        .getAdapter( IObservation.class );
    if( obs == null )
      return;

    Calendar c = Calendar.getInstance();
    Date to = c.getTime();
    c.add( Calendar.DAY_OF_YEAR, -31 );
    Date from = c.getTime();

    synchronized( obs )
    {
      m_template.setObservation( obs, false, new DateRangeArgument( from, to ) );
    }

    //    Job job = new ShowObservationJob( obs );
    //    job.schedule();
  }

  /**
   * @see org.eclipse.ui.IPartListener#partActivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partActivated( IWorkbenchPart part )
  {
    if( part != null && part instanceof RepositoryExplorerPart )
      ( (RepositoryExplorerPart)part ).addSelectionChangedListener( this );
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
      ( (RepositoryExplorerPart)part ).removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partDeactivated(org.eclipse.ui.IWorkbenchPart)
   */
  public void partDeactivated( IWorkbenchPart part )
  {
    if( part != null && part instanceof RepositoryExplorerPart )
      ( (RepositoryExplorerPart)part ).removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
   */
  public void partOpened( IWorkbenchPart part )
  {
  // Siehe partActivated...
  }

  //
  //  /**
  //   * Specific job for showing observation in table quickview.
  //   *
  //   * @author schlienger
  //   */
  //  private class ShowObservationJob extends Job
  //  {
  //    private final IObservation m_obs;
  //
  //    public ShowObservationJob( final IObservation obs )
  //    {
  //      super( "Aktualisierung von Tabelle-QuickView" );
  //
  //      m_obs = obs;
  //
  //      setPriority( Job.SHORT );
  //    }
  //
  //    /**
  //     * @see
  // org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
  //     */
  //    protected IStatus run( IProgressMonitor monitor )
  //    {
  //      Calendar c = Calendar.getInstance();
  //      Date to = c.getTime();
  //      c.add( Calendar.DAY_OF_YEAR, -31 );
  //      Date from = c.getTime();
  //
  //      m_template.setObservation( m_obs, false, new DateRangeArgument( from, to )
  // );
  //
  //      return Status.OK_STATUS;
  //    }
  //  }
}