package org.kalypso.ogc.sensor.view;

import java.awt.Font;
import java.awt.Frame;

import org.eclipse.jface.dialogs.MessageDialog;
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
import org.jfree.chart.title.TextTitle;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.impl.ObservationDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * Diagram QuickView.
 * 
 * @author schlienger
 */
public class DiagramViewPart extends ViewPart implements
    ISelectionChangedListener, IPartListener
{
  protected final ObservationDiagramTemplate m_template = new ObservationDiagramTemplate();

  private ObservationChart m_chart;

  private TextTitle m_subTitle;

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    try
    {
      m_chart = new ObservationChart( m_template );
    }
    catch( SensorException e )
    {
      MessageDialog.openError( parent.getShell(), "", e.getLocalizedMessage() );
      return;
    }

    m_template.addTemplateEventListener( m_chart );

    m_subTitle = new TextTitle("", new Font( "Default", Font.PLAIN, 12));
    m_chart.addSubtitle( m_subTitle );
    
    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
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

    if( m_chart != null )
      m_chart.dispose();

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
    m_template.removeAllThemes();

    final StructuredSelection selection = (StructuredSelection) event
        .getSelection();

    if( !(selection.getFirstElement() instanceof IRepositoryItem) )
      return;

    final IRepositoryItem item = (IRepositoryItem) selection.getFirstElement();

    final IObservation obs = ObservationCache.getInstance().getObservationFor(
        item );

    if( obs != null )
    {
      final DateRangeArgument dra = ObservationViewHelper.makeDateRange( item );

      m_template.setObservation( obs, dra );

      // sub title of diagram contains date-range info
      m_subTitle.setText("");
      if( dra != null )
        m_subTitle.setText( dra.toString() );
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