package org.kalypso.ogc.sensor.jface;

import java.awt.Frame;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
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
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.eclipse.core.resources.IProjectProvider;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationTimeSeries;
import org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.rules.Rules;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.BorrowObjectJob;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author belger
 */
public class ObservationLinkDialog extends TitleAreaDialog implements IPoolListener
{
  private final static Object DUMMY_OBJECT = new Object();

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool( IObservation.class );

  private final ObservationTableModel m_model = new ObservationTableModel();
  
  private IPoolableObjectType m_key;

  private final TimeseriesLink m_timeserie;

  private final TimeSeriesCollection m_tsCol = new TimeSeriesCollection();

  public ObservationLinkDialog( final Shell parentShell, final TimeseriesLink obslink,
      final IProjectProvider projectProvider )
  {
    super( parentShell );

    setShellStyle( getShellStyle() | SWT.RESIZE );

    m_timeserie = obslink;

    if( obslink != null )
      m_key = new PoolableObjectType( obslink.getLinktype(), obslink.getHref(), projectProvider.getProject() );
    startLoadTimeserie();
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

      final IAxis timeaxis = ObservationUtilities.findAxis( obs.getAxisList(), m_timeserie.getTimeaxis() );
      final IAxis valueaxis = ObservationUtilities.findAxis( obs.getAxisList(), m_timeserie.getValueaxis() );

      final List series = new ObservationTimeSeries( obs, timeaxis, new IAxis[]
      { valueaxis }, null ).getSeries();

      for( final Iterator it = series.iterator(); it.hasNext(); )
        m_tsCol.addSeries( (TimeSeries)it.next() );
      
      try
      {
        final DefaultTableViewTemplate tab = new DefaultTableViewTemplate( );
        tab.setObservation( obs, false, null );
        
        // TODO: marc, check if still ok since refactoring...
        m_model.setColumns( tab.getColumns(), null );
      }
      catch( final SensorException e )
      {
        // TODO error handling
        e.printStackTrace();
      }
    }
    else
    {
      if( m_key.equals( key ) )
      {
        if( bCannotReload )
        {
          m_tsCol.removeAllSeries();
          m_model.clearColumns();
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
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );

    final Frame vFrame = SWT_AWT.new_Frame( composite );

    final JFreeChart m_chart = ChartFactory
        .createTimeSeriesChart( "", "Datum", "Wert", m_tsCol, false, false, false );

    final ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );
  }

  private void createTable( final Composite parent )
  {
    final JTable table = new JTable( m_model );
    table.setDefaultRenderer( Date.class, new DateTableCellRenderer() );
    
    // TODO: habe MaskedNumber.class durch Double.class ersetzt. OK?
    table.setDefaultRenderer( Double.class, new MaskedNumberTableCellRenderer() );
    
    m_model.setRules( new Rules() );

    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );
    vFrame.setVisible( true );
    table.setVisible( true );
    vFrame.add( new JScrollPane( table ) );
  }
}