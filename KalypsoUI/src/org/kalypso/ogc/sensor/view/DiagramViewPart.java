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
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.part.ViewPart;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.jfreechart.ObservationTimeSeries;
import org.kalypso.util.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 *
 */
public class DiagramViewPart extends ViewPart implements ISelectionChangedListener
{
  private JFreeChart m_chart = null;
  private final TimeSeriesCollection m_tsCol = new TimeSeriesCollection();

  public DiagramViewPart()
  {
    //
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    m_chart = ChartFactory.createTimeSeriesChart( "QuickView", "Datum", "Wert", m_tsCol, false, false, false );
    
    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable(true, false);
    
    // SWT-AWT Brücke für die Darstellung von JFreeChart   
    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );
    
    vFrame.setVisible( true );
    chartPanel.setVisible( true );    
    vFrame.add( chartPanel );
    
    // sich bei der RepositoryView als SelectionListener anmelden
    IViewPart part = getViewSite().getPage().findView( "org.kalypso.util.repository.view.RepositoryExplorerPart" );
    if( part != null && part instanceof RepositoryExplorerPart )
      ((RepositoryExplorerPart)part).addSelectionChangedListener( this );
    
    // TODO: sich in IPartService.addPartListener() anmelden!
    // um reagieren zu können wenn die RepositoryView später hinzugefügt wird.
  }
  
  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    super.dispose();
    
    // TODO: NullPointerException hier irgendwo...
    IViewPart part = getViewSite().getPage().findView( "org.kalypso.util.repository.view.RepositoryExplorerPart" );
    if( part != null && part instanceof RepositoryExplorerPart )
      ((RepositoryExplorerPart)part).removeSelectionChangedListener( this );
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
    m_tsCol.removeAllSeries();
    
    StructuredSelection selection = (StructuredSelection)event.getSelection();
    
    if( !( selection.getFirstElement() instanceof IObservation ) )
      return;
    
    Calendar c = Calendar.getInstance();
    
    Date to = c.getTime();
    c.add( Calendar.DAY_OF_YEAR, -30 );
    Date from = c.getTime();
    
    try
    {
      m_tsCol.addSeries( new ObservationTimeSeries( (IObservation) selection.getFirstElement(), from, to ) );
    }
    catch( SensorException e )
    {
      e.printStackTrace();
    }
  }
}
