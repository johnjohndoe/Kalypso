package org.kalypso.ogc.sensor.view;

import java.awt.Frame;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.adapter.IAdaptable;
import org.kalypso.util.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 *  
 */
public class DiagramViewPart extends ViewPart implements ISelectionChangedListener, IPartListener
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
    m_chart = ChartFactory.createTimeSeriesChart( "", "Datum", "Wert", m_tsCol, false,
        false, false );

    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    // SWT-AWT Br�cke f�r die Darstellung von JFreeChart
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
    m_tsCol.removeAllSeries();

    StructuredSelection selection = (StructuredSelection)event.getSelection();

    if( !( selection.getFirstElement() instanceof IAdaptable ) )
      return;
    
    IObservation obs = (IObservation)((IAdaptable)selection.getFirstElement()).getAdapter( IObservation.class );
    if( obs == null )
      return;

    getViewSite().getShell().getDisplay().asyncExec( new ShowObservationInDiagramJob( m_tsCol, obs ) );
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
}
