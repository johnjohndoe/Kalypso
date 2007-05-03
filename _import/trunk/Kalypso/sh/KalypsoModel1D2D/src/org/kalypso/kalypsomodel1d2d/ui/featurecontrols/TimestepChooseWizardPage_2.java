package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
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
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class TimestepChooseWizardPage_2 extends WizardPage
{
  private Date m_dateFrom = new Date();

  private Date m_dateTo = new Date();

  protected DiagView m_diagView;

  protected TextTitle m_subTitle;

  private Text m_dateFromTxt;

  private Text m_dateToTxt;

  private IObservation m_observation;
  
  private static final DateFormat DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm");

  public TimestepChooseWizardPage_2( String title )
  {
    super( title );
  }

  public TimestepChooseWizardPage_2( )
  {
    super( "Super title" );
    setTitle( "Some title that should be changed." );
    setDescription( "Some subtitle that should be changed." );
  }

  public void init( IFolder currentSzenario, String parentFolder )
  {
    setPageComplete( false );
  }

  public void createControl( Composite parent )
  {
    
    makeComposite(parent);
  }
  
    private void makeComposite( Composite parent )
  {
      final Composite topComposite = new Composite( parent, SWT.NONE );
      GridLayout gridLayout = new GridLayout ();
      gridLayout.numColumns = 4;
      gridLayout.makeColumnsEqualWidth = true;
      topComposite.setLayout (gridLayout);
      GridData data = new GridData();
      
      Label vonLbl = new Label(topComposite, SWT.NONE);
      
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
