package org.kalypso.ogc.sensor.view;

import java.awt.Component;
import java.awt.Frame;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

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
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.swing.ObservationTableModel;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.util.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 *  
 */
public class TableViewPart extends ViewPart implements ISelectionChangedListener, IPartListener
{
  protected final ObservationTableModel m_model = new ObservationTableModel();
  
  
  public TableViewPart()
  {
  //
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    JTable table = new JTable( m_model );
    table.setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    table.setDefaultRenderer( Double.class, new DoubleTableCellRenderer() );
    
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
    m_model.setObservation( null, null );
    
    StructuredSelection selection = (StructuredSelection)event.getSelection();

    if( !( selection.getFirstElement() instanceof IObservation ) )
      return;

    Job job = new ShowObservationJob( (IObservation)selection.getFirstElement() );
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
      ((RepositoryExplorerPart)part).removeSelectionChangedListener( this );
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
   * 
   * @author schlienger
   */
  private class ShowObservationJob extends Job
  {
    private final IObservation m_obs;
    
    public ShowObservationJob( final IObservation obs )
    {
      super( "Table QuickView Update" );
      
      m_obs = obs;
      
      setPriority( Job.SHORT );
    }
    
    /**
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
     */
    protected IStatus run( IProgressMonitor monitor )
    {
      try
      {
        Calendar c = Calendar.getInstance();
        Date to = c.getTime();
        c.add( Calendar.DAY_OF_YEAR, -31 );
        Date from = c.getTime();

        m_model.setObservation( m_obs, m_obs.getValues(from, to) );
      }
      catch( SensorException e )
      {
        return new Status( IStatus.ERROR,
            KalypsoGisPlugin.getDefault().getBundle().getSymbolicName(), 0,
            "Fehler während die Aktualisierung des Diagramms", e );
      }
      
      return Status.OK_STATUS;
    }
  }
  
  /**
   * Helper: formatiert das Datum auf eine richtige Art und Weise
   * 
   * @author schlienger
   */
  private static class DateTableCellRenderer extends DefaultTableCellRenderer
  {
    private final static DateFormat df = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );
    
    /**
     * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
     */
    public Component getTableCellRendererComponent( JTable table, Object value, boolean isSelected,
        boolean hasFocus, int row, int column )
    {
      JLabel label = (JLabel)super.getTableCellRendererComponent( table, value, isSelected, hasFocus, row, column );
      
      label.setText( df.format( value ) );
      
      return label;
    }
  }
  
  private static class DoubleTableCellRenderer extends DefaultTableCellRenderer
  {
    private final static NumberFormat nf = NumberFormat.getInstance();
    
    /**
     * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
     */
    public Component getTableCellRendererComponent( JTable table, Object value, boolean isSelected,
        boolean hasFocus, int row, int column )
    {
      JLabel label = (JLabel)super.getTableCellRendererComponent( table, value, isSelected, hasFocus, row, column );
      
      label.setText( nf.format(value) );
      
      return label;
    }
  }
}
