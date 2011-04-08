package org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview;

import java.awt.Font;
import java.awt.Frame;
import java.io.File;
import java.net.MalformedURLException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.jfree.chart.title.TextTitle;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationTokenHelper;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.provider.PlainObsProvider;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public abstract class ZmlChooserControl
{
  public class ZmlFilter extends ViewerFilter
  {
    @Override
    public boolean select( final Viewer viewer, final Object parentElement, final Object element )
    {
      if( element instanceof File )
        return ((File) element).getName().endsWith( ".zml" ); //$NON-NLS-1$
      return true;
    }
  }

  private Date m_dateFrom = new Date();

  private Date m_dateTo = new Date();

  protected DiagView m_diagView;

  protected TextTitle m_subTitle;

  private IFolder m_importFolder = null;

  private Text m_dateFromTxt;

  private Text m_dateToTxt;

  private IObservation m_observation;

  private static final DateFormat DATE_FORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm" ); //$NON-NLS-1$

  static
  {
    DATE_FORMAT.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
  }

  public ZmlChooserControl( final IFolder importFolder )
  {
    m_importFolder = importFolder;
  }

  public Control createControl( final Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    gridLayout.makeColumnsEqualWidth = true;
    topComposite.setLayout( gridLayout );

    final Composite innerComposite = new SashForm( topComposite, SWT.HORIZONTAL | SWT.NULL );
    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.horizontalSpan = 4;
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    innerComposite.setLayoutData( data );

    final TreeViewer treeViewer = new TreeViewer( innerComposite, SWT.BORDER );

    // TODO: do NOT use this file stuff! do NOT work against eclipse!
    // TODO: use the Repository Framework instead!
    treeViewer.setContentProvider( new FileTreeContentProvider() );
    treeViewer.setLabelProvider( new FileTreeLabelProvider() );
    treeViewer.setInput( new File( m_importFolder.getLocation().toOSString() ) );
    treeViewer.addFilter( new ZmlFilter() );

    final Button loadZmlBtn = new Button( topComposite, SWT.PUSH );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    data.grabExcessHorizontalSpace = false;
    loadZmlBtn.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl.2" ) ); //$NON-NLS-1$
    loadZmlBtn.setLayoutData( data );
    final IFolder importFolder = m_importFolder;
    loadZmlBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // TODO: ths is very strange. Where does the wizard put the files??
        final IWizardDescriptor wizardDesc = PlatformUI.getWorkbench().getNewWizardRegistry().findWizard( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard" ); //$NON-NLS-1$
        final Shell shell = topComposite.getShell();
        try
        {
          final IWorkbenchWizard wizard = wizardDesc.createWizard();
          wizard.init( PlatformUI.getWorkbench(), new StructuredSelection( importFolder ) );
          final WizardDialog dialog = new WizardDialog( shell, wizard );
          dialog.open();
        }
        catch( final CoreException e1 )
        {
          final IStatus status = e1.getStatus();
          ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl.4" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl.5" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
          KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        }
        treeViewer.refresh();
      }
    } );

    final Label vonLbl = new Label( topComposite, SWT.NONE );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    data.grabExcessHorizontalSpace = true;
    vonLbl.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl.6" ) ); //$NON-NLS-1$
    vonLbl.setAlignment( SWT.RIGHT );
    vonLbl.setLayoutData( data );

    m_dateFromTxt = new Text( topComposite, SWT.BORDER );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    m_dateFromTxt.setLayoutData( data );
    m_dateFromTxt.setEnabled( false );
    m_dateFromTxt.addModifyListener( new ModifyListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          // TODO: check for right time zone
          m_dateFrom = DATE_FORMAT.parse( m_dateFromTxt.getText() );
          dateIntervalChanged();
          setComplete( true );
        }
        catch( final ParseException e1 )
        {
          m_diagView.removeAllItems();
          setComplete( false );
        }
      }
    } );

    final Button dateFromBtn = new Button( topComposite, SWT.PUSH );
    dateFromBtn.setText( "..." ); //$NON-NLS-1$
    dateFromBtn.setEnabled( false );
    dateFromBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( topComposite.getShell(), m_dateFrom );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateFrom = calendarDialog.getDate();
          // TODO: check for right time zone
          m_dateFromTxt.setText( DATE_FORMAT.format( m_dateFrom ) );
          dateIntervalChanged();
        }
      }
    } );

    final Label bisLbl = new Label( topComposite, SWT.NONE );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 2;
    data.grabExcessHorizontalSpace = true;
    bisLbl.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl.8" ) ); //$NON-NLS-1$
    bisLbl.setAlignment( SWT.RIGHT );
    bisLbl.setLayoutData( data );

    m_dateToTxt = new Text( topComposite, SWT.BORDER );
    data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    m_dateToTxt.setLayoutData( data );
    m_dateToTxt.setEnabled( false );
    m_dateToTxt.addModifyListener( new ModifyListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          // TODO: check for right time zone
          m_dateTo = DATE_FORMAT.parse( m_dateToTxt.getText() );
          dateIntervalChanged();
          setComplete( true );
        }
        catch( final ParseException e1 )
        {
          // e1.printStackTrace();
          m_diagView.removeAllItems();
          setComplete( false );
        }
      }
    } );

    final Button dateToBtn = new Button( topComposite, SWT.PUSH );
    dateToBtn.setText( "..." ); //$NON-NLS-1$
    dateToBtn.setEnabled( false );
    dateToBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( dateToBtn.getShell(), m_dateTo );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateTo = calendarDialog.getDate();
          // TODO: check for right time zone
          m_dateToTxt.setText( DATE_FORMAT.format( m_dateTo ) );
          dateIntervalChanged();
        }
      }
    } );

    m_diagView = new DiagView( true );
    ObservationChart m_chart = null;
    try
    {
      m_chart = new ObservationChart( m_diagView );
    }
    catch( final SensorException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    m_subTitle = new TextTitle( "", new Font( "Default", Font.PLAIN, 12 ) ); //$NON-NLS-1$ //$NON-NLS-2$
    m_chart.addSubtitle( m_subTitle );

    final Frame vFrame = SWT_AWT.new_Frame( new Composite( innerComposite, SWT.RIGHT | SWT.BORDER | SWT.EMBEDDED | SWT.Paint ) );

    vFrame.add( ChartFactory.createChartPanel( m_chart ) );

    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        // always remove items first (we don't know which selection we get)
        m_diagView.removeAllItems();

        setComplete( false );

        final StructuredSelection selection = (StructuredSelection) event.getSelection();

        final File selectedFile = (File) selection.getFirstElement();

        try
        {
          m_observation = ZmlFactory.parseXML( selectedFile.toURL() );
        }
        catch( final MalformedURLException e )
        {
        }
        catch( final SensorException e )
        {
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }

        m_subTitle.setText( "" ); //$NON-NLS-1$
        if( m_observation != null )
        {
          DateRange dateRange = null;
          try
          {
            final ITupleModel values = m_observation.getValues( null );
            final IAxis[] axisList = values.getAxes();
            final IAxis axis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_DATE ); //$NON-NLS-1$
            m_dateFrom = (Date) values.get( 0, axis );
            m_dateTo = (Date) values.get( values.size() - 1, axis );
            dateRange = new DateRange( m_dateFrom, m_dateTo );
          }
          catch( final SensorException e )
          {
            e.printStackTrace();
            dateRange = new DateRange( new Date( 1, 1, 1 ), null );
          }
          m_subTitle.setText( getDateRangeAsString( dateRange ) );
          m_diagView.addObservation( new PlainObsProvider( m_observation, new ObservationRequest( dateRange ) ), ObservationTokenHelper.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null, true ) );

          m_dateFromTxt.setText( DATE_FORMAT.format( m_dateFrom ) );
          m_dateToTxt.setText( DATE_FORMAT.format( m_dateTo ) );
          m_dateFromTxt.setEnabled( true );
          m_dateToTxt.setEnabled( true );
          dateFromBtn.setEnabled( true );
          dateToBtn.setEnabled( true );
          setComplete( true );
        }
        else
        {
          m_dateFromTxt.setText( "" ); //$NON-NLS-1$
          m_dateToTxt.setText( "" ); //$NON-NLS-1$
          m_dateFromTxt.setEnabled( false );
          m_dateToTxt.setEnabled( false );
          dateFromBtn.setEnabled( false );
          dateToBtn.setEnabled( false );
          setComplete( false );
        }
      }
    } );
    topComposite.getShell().setMinimumSize( 400, 300 );

    return topComposite;
  }

  private void dateIntervalChanged( )
  {
    m_diagView.removeAllItems();
    final DateRange range = new DateRange( m_dateFrom, m_dateTo );
    m_subTitle.setText( getDateRangeAsString( range ) );
    m_diagView.addObservation( new PlainObsProvider( m_observation, new ObservationRequest( range ) ), ObservationTokenHelper.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null, true ) );
  }

  /**
   * helper method, that returns the {@link DateRange} as a {@link String} considering the specified timezone.
   */
  private String getDateRangeAsString( final DateRange range )
  {
    final String fromTxt = DATE_FORMAT.format( range.getFrom() );
    final String toTxt = DATE_FORMAT.format( range.getTo() );

    return fromTxt + " - " + toTxt; //$NON-NLS-1$
  }

  public Date getFromDate( )
  {
    return m_dateFrom;
  }

  public Date getToDate( )
  {
    return m_dateTo;
  }

  public IObservation getObservation( )
  {
    return m_observation;
  }

  protected abstract void setComplete( final boolean complete );

  public String getDomainComponentUrn( )
  {
    return Kalypso1D2DDictConstants.DICT_COMPONENT_TIME;
  }

  public String getValueComponentUrn( )
  {
    // TODO: this is not nice...
    final String type = ObservationUtilities.findAxesByClass( m_observation.getAxes(), Double.class )[0].getType();

    if( type.equals( ITimeseriesConstants.TYPE_WATERLEVEL ) ) //$NON-NLS-1$
      return Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL;
    else if( type.equals( ITimeseriesConstants.TYPE_DISCHARGE ) ) //$NON-NLS-1$
      return Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE;

    throw new IllegalStateException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl.0" ) ); //$NON-NLS-1$
  }
}
