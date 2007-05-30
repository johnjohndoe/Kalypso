/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogPageUtilitites;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Constructs a simple timeserie with a time columnd and a value column.
 * 
 * @author Gernot Belger
 */
public class TimeserieStepDescriptor implements IBoundaryConditionDescriptor
{
  protected static final DateFormat DATETIMEFORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );

  protected static final DateFormat DATEFORMAT = new SimpleDateFormat( "dd.MM.yyyy 00:00" );

  private static final String DEFAULTSTEP = "1";

  private static final String DEFAULT_VALUE = "20.0";

  protected final Date[] m_dates = new Date[2];

  private Integer m_stepValue;

  private BigDecimal m_value;

  private WizardPage m_page;

  private final String m_domainComponentUrn;

  private final String m_valueComponentUrn;

  private final String m_name;

  private int m_field = Calendar.HOUR;

  private static final String MSG_PAGE = "Auf dieser Seite kann die gewünschte Zeitreihe definiert werden.";

  public TimeserieStepDescriptor( final String name, final String domainComponentUrn, final String valueComponentUrn )
  {
    m_name = name;
    m_domainComponentUrn = domainComponentUrn;
    m_valueComponentUrn = valueComponentUrn;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.jface.wizard.IWizardPage)
   */
  public Control createControl( final Composite parent, final WizardPage page )
  {
    m_page = page;

    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout( 3, false );
    container.setLayout( gridLayout );

    createDateEntryLine( container, "Von:", 0, 0 );
    createDateEntryLine( container, "Bis:", 1, 24 );

    final Label stepLabel = new Label( container, SWT.NONE );
    stepLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    stepLabel.setText( "Zeitschritt:" );

    final Text dateTimeStep = new Text( container, SWT.BORDER );
    dateTimeStep.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    dateTimeStep.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = dateTimeStep.getText();
        handleStepChanged( text );
      }
    } );
    dateTimeStep.setText( DEFAULTSTEP );

    final ComboViewer comboViewer = new ComboViewer( container, SWT.DROP_DOWN | SWT.READ_ONLY );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        return ((FIELD) element).getAddLabel();
      }
    } );
    comboViewer.setInput( new Object[] { FIELD.WEEK_OF_YEAR, FIELD.DAY_OF_YEAR, FIELD.HOUR, FIELD.MINUTE } );
    comboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final int field = ((FIELD) ((IStructuredSelection) event.getSelection()).getFirstElement()).getField();
        handleFieldChanged( field );
      }
    } );
    comboViewer.setSelection( new StructuredSelection( FIELD.HOUR ), true );

    final Label labelValue = new Label( container, SWT.NONE );
    labelValue.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    labelValue.setText( "Standardwert:" );

    final Text defaultValue = new Text( container, SWT.BORDER );
    defaultValue.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    defaultValue.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = defaultValue.getText();
        handleValueChanged( text );
      }
    } );

    defaultValue.setText( DEFAULT_VALUE );

    new Label( container, SWT.NONE ).setText( "" );

    updatePageState( Status.OK_STATUS );

    return container;
  }

  private void createDateEntryLine( final Composite parent, final String label, final int index, final int dateOffset )
  {
    final Label labelFrom = new Label( parent, SWT.NONE );
    labelFrom.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    labelFrom.setText( label );

    final Text dateText = new Text( parent, SWT.BORDER );
    dateText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    dateText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = dateText.getText();
        handleDateChanged( index, text );
      }
    } );

    final Calendar cal = Calendar.getInstance();
    cal.add( m_field, dateOffset );
    dateText.setText( DATEFORMAT.format( cal.getTime() ) );

    final Button buttonDateTimeFrom = new Button( parent, SWT.NONE );
    buttonDateTimeFrom.setText( "..." );
    buttonDateTimeFrom.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( parent.getShell(), m_dates[index] );
        if( calendarDialog.open() == Window.OK )
        {
          final Date date = calendarDialog.getDate();
          dateText.setText( DATEFORMAT.format( date ) );
        }
      }
    } );

  }

  protected void updatePageState( final IStatus status )
  {
    final IStatus pageStatus = status.isOK() ? checkPageComplete() : status;
    if( pageStatus.isOK() )
      m_page.setMessage( MSG_PAGE, DialogPage.NONE );
    else
      m_page.setMessage( pageStatus.getMessage(), DialogPageUtilitites.severityToMessagecode( pageStatus ) );

    m_page.setPageComplete( pageStatus.isOK() );
  }

  private IStatus checkPageComplete( )
  {
    if( m_dates[0] == null )
      return StatusUtilities.createWarningStatus( "Geben Sie ein Startdatum ein." );
    if( m_dates[1] == null )
      return StatusUtilities.createWarningStatus( "Geben Sie ein Enddatum ein." );
    if( m_stepValue == null )
      return StatusUtilities.createWarningStatus( "Geben Sie eine Schrittweite ein." );
    if( m_value == null )
      return StatusUtilities.createWarningStatus( "Geben Sie einen Standardwert ein." );

    if( !m_dates[0].before( m_dates[1] ) )
      return StatusUtilities.createErrorStatus( "Startdatum muss vor Enddatum liegen." );

    return Status.OK_STATUS;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#activate()
   */
  public void activate( )
  {
    m_page.setTitle( "Zeitreihendefinition" );
    m_page.setDescription( MSG_PAGE );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#fillObservation(org.kalypso.observation.IObservation)
   */
  public void fillObservation( final IObservation<TupleResult> obs )
  {
    final TupleResult result = obs.getResult();

    obs.setName( getName() );
    obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) );

    final IComponent[] components = result.getComponents();
    final IComponent domainComponent = components[0];
    final IComponent valueComponent = components[1];
    result.setSortComponents( new IComponent[] { domainComponent } );

    final GregorianCalendar calendarFrom = new GregorianCalendar();
    final GregorianCalendar calendarTo = new GregorianCalendar();

    calendarFrom.setTime( m_dates[0] );
    calendarTo.setTime( m_dates[1] );

    while( !calendarFrom.after( calendarTo ) )
    {
      final IRecord record = result.createRecord();
      record.setValue( domainComponent, new XMLGregorianCalendarImpl( calendarFrom ) );
      record.setValue( valueComponent, m_value );
      result.add( record );
      calendarFrom.add( m_field, m_stepValue );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  protected void handleStepChanged( final String text ) throws NumberFormatException
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_stepValue = new Integer( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, "Formatfehler im Zeitschritt: geben Sie einen Ganzzahlwert ein" );
      m_stepValue = null;
    }

    updatePageState( status );
  }

  protected void handleValueChanged( final String text ) throws NumberFormatException
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_value = new BigDecimal( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, "Formatfehler im Standardwert: geben Sie eine Dezimalzahl ein" );
      m_value = null;
    }

    updatePageState( status );
  }

  protected void handleDateChanged( final int index, final String text )
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_dates[index] = DATETIMEFORMAT.parse( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, "Formatfehler in Datumswert. Geben Sie ein Datum wie folgt ein: " + DATEFORMAT.format( new Date() ) );

      m_dates[index] = null;
    }

    updatePageState( status );
  }

  protected void handleFieldChanged( final int field )
  {
    m_field = field;

    updatePageState( Status.OK_STATUS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getDomainComponentUrn()
   */
  public String getDomainComponentUrn( )
  {
    return m_domainComponentUrn;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getValueComponentUrn()
   */
  public String getValueComponentUrn( )
  {
    return m_valueComponentUrn;
  }

}
