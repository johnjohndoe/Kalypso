package org.kalypso.ogc.sensor.view;

import java.awt.Frame;
import java.util.Calendar;
import java.util.Date;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;
import org.kalypso.util.adapter.IAdaptable;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * Diagram QuickView.
 * 
 * @author schlienger
 */
public class DiagramViewPart extends ViewPart implements ISelectionChangedListener, IPartListener
{
  final protected DefaultDiagramTemplate m_template = new DefaultDiagramTemplate();

  private ObservationChart m_chart;

  //private JFreeChart m_chart = null;

  //protected final TimeSeriesCollection m_tsCol = new TimeSeriesCollection();

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    //    m_chart = ChartFactory
    //        .createTimeSeriesChart( "", "Datum", "Wert", m_tsCol, false, false, false
    // );

    try
    {
      m_chart = new ObservationChart( m_template );
    }
    catch( FactoryException e )
    {
      e.printStackTrace();
    }
    m_template.addTemplateEventListener( m_chart );

    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );

    getSite().getPage().addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    getSite().getPage().removePartListener( this );

    m_template.removeTemplateEventListener( m_chart );

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
    //m_tsCol.removeAllSeries();

    m_template.removeAllCurves();
    
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
      System.out.println( "DiagView enter " + obs );
      
      m_template.setObservation( obs, new DateRangeArgument( from, to ) );
      
      System.out.println( "DiagView exit " + obs );
    }

    //new ShowObservationJob( obs ).schedule();
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

//  /**
//   * Specific job for showing observation in diagram quickview.
//   * 
//   * @author schlienger
//   */
//  private class ShowObservationJob extends Job
//  {
//    private final IObservation m_obs;
//
//    public ShowObservationJob( final IObservation obs )
//    {
//      super( "Aktualisierung von Diagramm-QuickView" );
//
//      m_obs = obs;
//
//      setPriority( Job.SHORT );
//    }
//
//    /**
//     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
//     */
//    protected IStatus run( IProgressMonitor monitor )
//    {
//      Calendar c = Calendar.getInstance();
//
//      Date to = c.getTime();
//      c.add( Calendar.DAY_OF_YEAR, -31 );
//      Date from = c.getTime();
//
//      m_template.setObservation( m_obs, new DateRangeArgument( from, to ) );
//
//      return Status.OK_STATUS;
//    }
//  }
}

//    /**
//     * @see
// org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
//     */
//    protected IStatus run( IProgressMonitor monitor )
//    {
//      Calendar c = Calendar.getInstance();
//
//      Date to = c.getTime();
//      c.add( Calendar.DAY_OF_YEAR, -31 );
//      Date from = c.getTime();
//
//      try
//      {
//        // TODO: ok so mit der Collection von Series?
//        List series = new ObservationTimeSeries( m_obs, new DateRangeArgument( from,
// to ) )
//            .getSeries();
//
//        for( Iterator it = series.iterator(); it.hasNext(); )
//        {
//          TimeSeries s = (TimeSeries)it.next();
//
//          m_tsCol.addSeries( s );
//        }
//      }
//      catch( SensorException e )
//      {
//        return new Status( IStatus.WARNING, KalypsoGisPlugin.getId(), 0, "Fehler beim
// Laden der Diagrammdaten", e );
//      }
//      
//      return Status.OK_STATUS;
//    }
