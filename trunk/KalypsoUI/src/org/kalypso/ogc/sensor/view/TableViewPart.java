package org.kalypso.ogc.sensor.view;

import java.awt.Frame;
import java.util.Calendar;
import java.util.Date;

import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ogc.sensor.DateRangeArgument;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.template.DefaultTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.template.Rules;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.adapter.IAdaptable;
import org.kalypso.util.repository.view.RepositoryExplorerPart;
import org.kalypso.util.status.MaskedNumber;

/**
 * Table QuickView.
 * 
 * @author schlienger
 */
public class TableViewPart extends ViewPart implements ISelectionChangedListener, IPartListener
{
  protected final ObservationTableModel m_model = new ObservationTableModel();

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    JTable table = new JTable( m_model );
    table.setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    table.setDefaultRenderer( MaskedNumber.class, new MaskedNumberTableCellRenderer() );
    
    // TODO: als Testzweck hier, später komplett entfernen?
    m_model.setRules( new Rules() );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    table.setVisible( true );
    vFrame.add( new JScrollPane( table ) );

    getSite().getPage().addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    getSite().getPage().removePartListener( this );

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
    try
    {
      m_model.setColumns( null, null );
    }
    catch( SensorException e )
    {
      // TODO handling
      e.printStackTrace();
    }

    StructuredSelection selection = (StructuredSelection)event.getSelection();

    if( !( selection.getFirstElement() instanceof IAdaptable ) )
      return;

    IObservation obs = (IObservation)( (IAdaptable)selection.getFirstElement() )
        .getAdapter( IObservation.class );
    if( obs == null )
      return;

    Job job = new ShowObservationJob( obs );
    job.schedule();
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

  /**
   * Specific job for showing observation in table quickview.
   * 
   * @author schlienger
   */
  private class ShowObservationJob extends Job
  {
    private final IObservation m_obs;

    public ShowObservationJob( final IObservation obs )
    {
      super( "Aktualisierung von Tabelle-QuickView" );

      m_obs = obs;

      setPriority( Job.SHORT );
    }

    /**
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
     */
    protected IStatus run( IProgressMonitor monitor )
    {
      Calendar c = Calendar.getInstance();
      Date to = c.getTime();
      c.add( Calendar.DAY_OF_YEAR, -31 );
      Date from = c.getTime();

      DefaultTableViewTemplate tab = new DefaultTableViewTemplate( m_obs );

      try
      {
        m_model.setColumns( tab.getColumns(), new DateRangeArgument( from, to ) );
      }
      catch( SensorException e )
      {
        return new Status( IStatus.WARNING, KalypsoGisPlugin.getId(), 0, "Fehler beim Laden der Tabellendaten", e );
      }

      return Status.OK_STATUS;
    }
  }
}