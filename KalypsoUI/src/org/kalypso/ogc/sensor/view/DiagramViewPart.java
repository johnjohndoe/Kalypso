package org.kalypso.ogc.sensor.view;

import java.awt.Frame;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IProgressService;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * Diagram QuickView.
 * 
 * @author schlienger
 */
public class DiagramViewPart extends ViewPart implements
    ISelectionChangedListener, IPartListener
{
  protected final DefaultDiagramTemplate m_template = new DefaultDiagramTemplate();

  private ObservationChart m_chart;

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    try
    {
      m_chart = new ObservationChart( m_template );
    }
    catch( FactoryException e )
    {
      MessageDialog.openError( parent.getShell(), "", e.getLocalizedMessage() );
      return;
    }

    m_template.addTemplateEventListener( m_chart );

    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    // SWT-AWT Br�cke f�r die Darstellung von JFreeChart
    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );

    getSite().getPage().addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose( )
  {
    getSite().getPage().removePartListener( this );

    m_template.removeTemplateEventListener( m_chart );

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
    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor )
      {
        monitor.beginTask( "DiagramView Update", 2 );
        
        m_template.removeAllCurves();

        final StructuredSelection selection = (StructuredSelection) event
            .getSelection();

        if( !(selection.getFirstElement() instanceof IRepositoryItem) )
          return;

        final IRepositoryItem item = (IRepositoryItem) selection
            .getFirstElement();

        final IObservation obs = ObservationCache.getObservationFor( item );
        if( obs != null )
        {
          final int days = Integer.valueOf(
              item.getRepository().getProperty(
                  IKalypsoPreferences.NUMBER_OF_DAYS ) ).intValue();

          m_template.setObservation( obs, DateRangeArgument
              .createFromPastDays( days ) );
        }
        
        monitor.done();
      }
    };

    try
    {
      final IProgressService service = PlatformUI.getWorkbench().getProgressService();
      service.busyCursorWhile( runnable );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
    }
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