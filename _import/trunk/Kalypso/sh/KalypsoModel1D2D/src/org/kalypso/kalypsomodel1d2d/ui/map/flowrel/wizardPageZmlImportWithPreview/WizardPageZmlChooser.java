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
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.jfree.chart.title.TextTitle;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.wizards.imports.observation.ImportObservationWizard;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class WizardPageZmlChooser extends WizardPage
{
  public class ZmlFilter extends ViewerFilter
  {

    public ZmlFilter( )
    {
      super();
    }

    @Override
    public boolean select( Viewer viewer, Object parentElement, Object element )
    {
      if( element instanceof File )
        return ((File) element).getName().endsWith( ".zml" );
      return true;
    }
  }

  private Date m_dateFrom = new Date();

  private Date m_dateTo = new Date();

  protected DiagView m_diagView;

  protected TextTitle m_subTitle;

  private String m_parentFolder = "";
  
  private IFolder m_currentSzenario;

  private Text m_dateFromTxt;

  private Text m_dateToTxt;

  private IObservation m_observation;
  
  private static final DateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm");

  public WizardPageZmlChooser( String title )
  {
    super( title );
  }

  public WizardPageZmlChooser( )
  {
    super( "Super title" );
    setTitle( "Some title that should be changed." );
    setDescription( "Some subtitle that should be changed." );
  }

  public void init( IFolder currentSzenario, String parentFolder )
  {
    m_currentSzenario = currentSzenario;
    m_parentFolder = parentFolder;
    setPageComplete( false );
  }

  public void createControl( Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NONE );
    GridLayout gridLayout = new GridLayout ();
    gridLayout.numColumns = 4;
    gridLayout.makeColumnsEqualWidth = true;
    topComposite.setLayout (gridLayout);
    
    Composite innerComposite = new SashForm( topComposite, SWT.HORIZONTAL | SWT.NULL );
    GridData data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.horizontalSpan = 4;
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    innerComposite.setLayoutData (data);

    final TreeViewer treeViewer = new TreeViewer( innerComposite );
    treeViewer.setContentProvider( new FileTreeContentProvider() );
    treeViewer.setLabelProvider( new FileTreeLabelProvider() );
    treeViewer.setInput( new File( m_parentFolder ) );
    treeViewer.addFilter( new ZmlFilter() );
    
    Button loadZmlBtn = new Button(topComposite, SWT.PUSH);
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    data.grabExcessHorizontalSpace = false;
    loadZmlBtn.setText( "Importieren" );
    loadZmlBtn.setLayoutData( data );
    loadZmlBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final ImportObservationWizard wizard = new ImportObservationWizard();
        wizard.setProject( m_currentSzenario.getProject() );
        final WizardDialog dialog = new WizardDialog( topComposite.getShell(), wizard );
        dialog.open();
        treeViewer.refresh();
      }
    } );

    Label vonLbl = new Label(topComposite, SWT.NONE);
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 1;
    data.grabExcessHorizontalSpace = true;
    vonLbl.setText( "Von:" );
    vonLbl.setAlignment( SWT.RIGHT );
    vonLbl.setLayoutData( data );
    
    m_dateFromTxt = new Text( topComposite, SWT.BORDER );
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    m_dateFromTxt.setLayoutData (data);
    m_dateFromTxt.setEnabled( false );
    m_dateFromTxt.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        try
        {
          m_dateFrom = DATE_FORMAT.parse( m_dateFromTxt.getText());
          dateIntervalChanged();
          setPageComplete( true );
        }
        catch( ParseException e1 )
        {
          m_diagView.removeAllItems();
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
      }
    } );

    final Button dateFromBtn = new Button( topComposite, SWT.PUSH );
    dateFromBtn.setText( "..." );
    dateFromBtn.setEnabled( false );
    dateFromBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( getShell(), m_dateFrom );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateFrom = calendarDialog.getDate();
          m_dateFromTxt.setText( DATE_FORMAT.format( m_dateFrom) );
          dateIntervalChanged();
        }
      }
    } );

    Label bisLbl = new Label(topComposite, SWT.NONE);
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    data.horizontalSpan = 2;
    data.grabExcessHorizontalSpace = true;
    bisLbl.setText( "Bis:" );
    bisLbl.setAlignment( SWT.RIGHT );
    bisLbl.setLayoutData( data );
    
    m_dateToTxt = new Text( topComposite, SWT.BORDER );
    data = new GridData ();
    data.horizontalAlignment = GridData.FILL;
    m_dateToTxt.setLayoutData (data);
    m_dateToTxt.setEnabled( false );
    m_dateToTxt.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        try
        {
          m_dateTo = DATE_FORMAT.parse( m_dateToTxt.getText());
          dateIntervalChanged();
          setPageComplete( true );
        }
        catch( ParseException e1 )
        {
//          e1.printStackTrace();
          m_diagView.removeAllItems();
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
      }
    } );


    final Button dateToBtn = new Button( topComposite, SWT.PUSH );
    dateToBtn.setText( "..." );
    dateToBtn.setEnabled( false );
    dateToBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( getShell(), m_dateTo );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateTo = calendarDialog.getDate();
          m_dateToTxt.setText( DATE_FORMAT.format( m_dateTo) );
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
    catch( SensorException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    m_subTitle = new TextTitle( "", new Font( "Default", Font.PLAIN, 12 ) );
    m_chart.addSubtitle( m_subTitle );
    
    
    
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( innerComposite, SWT.RIGHT | SWT.BORDER | SWT.EMBEDDED | SWT.Paint ) );
    
    vFrame.add( ChartFactory.createChartPanel( m_chart ) );
    
    treeViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        // always remove items first (we don't know which selection we get)
        m_diagView.removeAllItems();

        setPageComplete( false );

        final StructuredSelection selection = (StructuredSelection) event.getSelection();

        File selectedFile = (File) selection.getFirstElement();

        try
        {
          m_observation = ZmlFactory.parseXML( selectedFile.toURL(), selectedFile.getAbsolutePath() );
        }
        catch( MalformedURLException e )
        {
        }
        catch( SensorException e )
        {
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }

        m_subTitle.setText( "" );
        if( m_observation != null )
        {
          DateRange dateRange = null;
          try
          {
            final ITuppleModel values = m_observation.getValues( null );
            final IAxis[] axisList = values.getAxisList();
            final IAxis axis = ObservationUtilities.findAxisByType( axisList, "date" );
            m_dateFrom = (Date) values.getElement( 0, axis );
            m_dateTo = (Date) values.getElement( values.getCount() - 1, axis );
            dateRange = new DateRange( m_dateFrom, m_dateTo );
          }
          catch( SensorException e )
          {
            e.printStackTrace();
            dateRange = new DateRange( new Date( 1, 1, 1 ), null );
          }
          m_subTitle.setText( dateRange.toString() );
          m_diagView.addObservation( new PlainObsProvider( m_observation, new ObservationRequest( dateRange ) ), ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
          m_dateFromTxt.setText( DATE_FORMAT.format( m_dateFrom) );
          m_dateToTxt.setText( DATE_FORMAT.format( m_dateTo) );
          m_dateFromTxt.setEnabled( true );
          m_dateToTxt.setEnabled( true );
          dateFromBtn.setEnabled( true );
          dateToBtn.setEnabled( true );
          setPageComplete( true );
        }
        else
        {
          m_dateFromTxt.setText( "" );
          m_dateToTxt.setText( "" );
          m_dateFromTxt.setEnabled( false );
          m_dateToTxt.setEnabled( false );
          dateFromBtn.setEnabled( false );
          dateToBtn.setEnabled( false );
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
//        getWizard().canFinish();
      }
    } );
    topComposite.getShell().setMinimumSize( 400, 300 );
    setControl( topComposite );

    parent.pack();
    parent.layout();
  }
  
  private void dateIntervalChanged() {
    m_diagView.removeAllItems();
    DateRange range = new DateRange( m_dateFrom, m_dateTo );
    m_subTitle.setText( range.toString() );
    m_diagView.addObservation( new PlainObsProvider( m_observation, new ObservationRequest( range ) ), ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
  }
  
  public Date getFromDate() {
    return m_dateFrom;
  }
  
  public Date getToDate() {
    return m_dateTo;
  }
  
  public ITuppleModel getTuppleModel() {
    try
    {
      return m_observation.getValues( null );
    }
    catch( SensorException e )
    {
      e.printStackTrace();
      return null;
    }
  }
  
  public String[] getComponentUrns() {
    String[] res = new String[2];
    res[0] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time";
    res[1] = "";
    String type = m_observation.getAxisList()[1].getType();
    if(type.equals( "W" ))
      res[1] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel";
    if(type.equals( "Q" ))
      res[1] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge";
    return res;
  }
}
