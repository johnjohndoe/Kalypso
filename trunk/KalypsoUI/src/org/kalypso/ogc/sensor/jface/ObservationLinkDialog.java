package org.kalypso.ogc.sensor.jface;

import java.awt.Frame;
import java.net.URL;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.diagview.impl.DefaultDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author belger
 */
public class ObservationLinkDialog extends TitleAreaDialog implements IPoolListener
{
  private final static Object DUMMY_OBJECT = new Object();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool( IObservation.class );

  private IPoolableObjectType m_key;

  private final TimeseriesLink m_timeserie;

  private final DefaultDiagramTemplate m_diagTemplate = new DefaultDiagramTemplate();
  private ObservationChart m_chart;
  
  private final DefaultTableViewTemplate m_tableTemplate = new DefaultTableViewTemplate();
  private ObservationTable m_table;
  
  
  public ObservationLinkDialog( final Shell parentShell, final TimeseriesLink obslink,
      final URL context )
  {
    super( parentShell );

    setShellStyle( getShellStyle() | SWT.RESIZE );

    m_timeserie = obslink;

    if( obslink != null )
      m_key = new PoolableObjectType( obslink.getLinktype(), obslink.getHref(), context );
    startLoadTimeserie();
  }

  public void dispose()
  {
    m_diagTemplate.removeTemplateEventListener( m_chart );
    m_tableTemplate.removeTemplateEventListener( m_table );
  }
  
  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#getInitialSize()
   */
  protected Point getInitialSize()
  {
    return new Point( 500, 400 );
  }

  private void startLoadTimeserie()
  {
    if( m_key != null )
    {
      final Job job = new BorrowObjectJob( "Zeitreihe laden", m_pool, this, m_key, DUMMY_OBJECT );
      job.schedule();
    }
  }

  public TimeseriesLink getResult()
  {
    return m_timeserie;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#onObjectInvalid(org.kalypso.util.pool.ResourcePool,
   *      org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, boolean)
   */
  public void onObjectInvalid( final ResourcePool source, final IPoolableObjectType key,
      final Object oldObject, final boolean bCannotReload ) throws Exception
  {
    if( oldObject == DUMMY_OBJECT )
    {
      final IObservation obs = (IObservation)m_pool.getObject( key, new NullProgressMonitor() );
      
      final int days = KalypsoGisPlugin.getDefault().getPluginPreferences().getInt( IKalypsoPreferences.NUMBER_OF_DAYS );
      
      //m_diagTemplate.removeAllCurves();
      m_diagTemplate.setObservation( obs, DateRangeArgument.createFromPastDays( days ) );
      
      //m_tableTemplate.removeAllColumns();
      m_tableTemplate.setObservation( obs, false, DateRangeArgument.createFromPastDays( days ) );
    }
    else
    {
      if( m_key.equals( key ) )
      {
        if( bCannotReload )
        {
          m_diagTemplate.removeAllCurves();
          m_tableTemplate.removeAllColumns();
        }
        else
          startLoadTimeserie();
      }
    }
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    final Composite control = new Composite( parent, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.verticalSpacing = 50;
    control.setLayout( layout );
    final GridData titleGridData = new GridData( GridData.FILL_HORIZONTAL );
    final GridData layoutGridData = titleGridData;
    control.setLayoutData( layoutGridData );
    control.setFont( parent.getFont() );

    // Build the separator line
    final Label titleBarSeparator = new Label( control, SWT.HORIZONTAL | SWT.SEPARATOR );
    titleBarSeparator.setLayoutData( titleGridData );

    final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );
    sashForm.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    
    createDiagram( sashForm );
    createTable( sashForm );

    setMessage( "Zeitreihenansicht" );
    
    return control;
  }

  private void createDiagram( final Composite parent )
  {
    try
    {
      m_chart = new ObservationChart( m_diagTemplate );
    }
    catch( FactoryException e )
    {
      MessageDialog.openError( parent.getShell(), "", e.getLocalizedMessage() );
      return;
    }
    
    m_diagTemplate.addTemplateEventListener( m_chart );

    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );
  }

  private void createTable( final Composite parent )
  {
    m_table = new ObservationTable( new ObservationTableModel() );
    m_tableTemplate.addTemplateEventListener( m_table );
    
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    m_table.setVisible( true );

    final JScrollPane pane = new JScrollPane( m_table );
    pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );
  }
}