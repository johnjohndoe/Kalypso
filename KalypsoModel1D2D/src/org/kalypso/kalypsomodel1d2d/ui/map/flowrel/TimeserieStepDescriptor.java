/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import java.util.TimeZone;

import javax.xml.datatype.XMLGregorianCalendar;

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
import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogPageUtilitites;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Constructs a simple timeserie with a time column and a value column.
 * 
 * @author Gernot Belger
 */
public class TimeserieStepDescriptor implements IBoundaryConditionDescriptor
{
  protected static final DateFormat DATETIMEFORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss.SSS" ); //$NON-NLS-1$

  protected static final DateFormat DATEFORMAT = new SimpleDateFormat( "dd.MM.yyyy 00:00:00.000" ); //$NON-NLS-1$

  private static final String DEFAULTSTEP = "1"; //$NON-NLS-1$

  private static final String DEFAULT_VALUE = "20.0"; //$NON-NLS-1$

  protected final Date[] m_dates = new Date[2];

  private Integer m_stepValue;

  private BigDecimal m_fromValue;

  private BigDecimal m_toValue;

  private WizardPage m_page;

  private final String m_domainComponentUrn;

  private final String m_valueComponentUrn;

  private final String m_name;

  private int m_field = Calendar.MINUTE;

  private static final String MSG_PAGE = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.4" ); //$NON-NLS-1$

  public TimeserieStepDescriptor( final String name, final String domainComponentUrn, final String valueComponentUrn )
  {
    m_name = name;
    m_domainComponentUrn = domainComponentUrn;
    m_valueComponentUrn = valueComponentUrn;
    TimeZone timeZone = KalypsoCorePlugin.getDefault().getTimeZone();
    DATETIMEFORMAT.setTimeZone( timeZone );
    DATEFORMAT.setTimeZone( timeZone );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.jface.wizard.IWizardPage)
   */
  @Override
  public Control createControl( final Composite parent, final WizardPage page )
  {
    m_page = page;

    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout( 3, false );
    container.setLayout( gridLayout );

    createDateEntryLine( container, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.5" ), 0, 0 ); //$NON-NLS-1$
    createDateEntryLine( container, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.6" ), 1, 24 ); //$NON-NLS-1$

    final Label stepLabel = new Label( container, SWT.NONE );
    stepLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    stepLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.7" ) ); //$NON-NLS-1$

    final Text dateTimeStep = new Text( container, SWT.BORDER );
    dateTimeStep.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    dateTimeStep.addModifyListener( new ModifyListener()
    {
      @Override
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
    comboViewer.setInput( new Object[] { FIELD.WEEK_OF_YEAR, FIELD.DAY_OF_YEAR, FIELD.HOUR, FIELD.MINUTE, FIELD.SECOND, FIELD.MILLISECOND } );
    comboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final int field = ((FIELD) ((IStructuredSelection) event.getSelection()).getFirstElement()).getField();
        handleFieldChanged( field );
      }
    } );
    comboViewer.setSelection( new StructuredSelection( FIELD.MINUTE ), true );

    final Label labelFromValue = new Label( container, SWT.NONE );
    labelFromValue.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    labelFromValue.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.8" ) ); //$NON-NLS-1$

    final Text defaultFromValue = new Text( container, SWT.BORDER );
    defaultFromValue.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    defaultFromValue.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String text = defaultFromValue.getText();
        handleFromValueChanged( text );
      }
    } );

    defaultFromValue.setText( DEFAULT_VALUE );

    new Label( container, SWT.NONE ).setText( "" ); //$NON-NLS-1$

    final Label labelToValue = new Label( container, SWT.NONE );
    labelToValue.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    labelToValue.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.10" ) ); //$NON-NLS-1$

    final Text defaultToValue = new Text( container, SWT.BORDER );
    defaultToValue.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    defaultToValue.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String text = defaultToValue.getText();
        handleToValueChanged( text );
      }
    } );

    defaultToValue.setText( DEFAULT_VALUE );

    new Label( container, SWT.NONE ).setText( "" ); //$NON-NLS-1$

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
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String text = dateText.getText();
        handleDateChanged( index, text );
      }
    } );

    final Calendar cal = Calendar.getInstance();
    cal.add( Calendar.HOUR, dateOffset );
    dateText.setText( DATEFORMAT.format( cal.getTime() ) );

    final Button buttonDateTimeFrom = new Button( parent, SWT.NONE );
    buttonDateTimeFrom.setText( "..." ); //$NON-NLS-1$
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
      return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.13" ) ); //$NON-NLS-1$
    if( m_dates[1] == null )
      return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.14" ) ); //$NON-NLS-1$
    if( m_stepValue == null )
      return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.15" ) ); //$NON-NLS-1$
    if( m_fromValue == null )
      return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.16" ) ); //$NON-NLS-1$
    if( m_toValue == null )
      return StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.17" ) ); //$NON-NLS-1$

    if( !m_dates[0].before( m_dates[1] ) )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.18" ) ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

  protected BigDecimal interpolateValue( final GregorianCalendar calendar )
  {
    /* Linear interpolation */
    final XMLGregorianCalendar from = DateUtilities.toXMLGregorianCalendar( m_dates[0] );
    final XMLGregorianCalendar to = DateUtilities.toXMLGregorianCalendar( m_dates[1] );

    final long before = from.toGregorianCalendar().getTimeInMillis();
    final long after = to.toGregorianCalendar().getTimeInMillis();

    final long dom = calendar.getTimeInMillis();

    final double valBefore = ((Number) m_fromValue).doubleValue();
    final double valAfter = ((Number) m_toValue).doubleValue();

    try
    {
      final LinearEquation equation = new LinearEquation( before, valBefore, after, valAfter );
      double value = equation.computeY( dom );

      return new BigDecimal( value ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    }
    catch( final SameXValuesException e )
    {
      e.printStackTrace();

      return null;
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#activate()
   */
  @Override
  public void activate( )
  {
    m_page.setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.19" ) ); //$NON-NLS-1$
    m_page.setDescription( MSG_PAGE );

    updatePageState( Status.OK_STATUS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#fillObservation(org.kalypso.observation.IObservation)
   */
  @Override
  public void fillObservation( final IObservation<TupleResult> obs )
  {
    final TupleResult result = obs.getResult();

    obs.setName( getName() );
    obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) ); //$NON-NLS-1$

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

      // TODO: interpolate value
      final BigDecimal value = interpolateValue( calendarFrom );

      if( value != null )
      {
        record.setValue( valueComponent, value );
        result.add( record );
      }
      calendarFrom.add( m_field, m_stepValue );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#getName()
   */
  @Override
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
      status = StatusUtilities.statusFromThrowable( t, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.21" ) ); //$NON-NLS-1$
      m_stepValue = null;
    }

    updatePageState( status );
  }

  protected void handleFromValueChanged( final String text ) throws NumberFormatException
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_fromValue = new BigDecimal( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.22" ) ); //$NON-NLS-1$
      m_fromValue = null;
    }

    updatePageState( status );
  }

  protected void handleToValueChanged( final String text ) throws NumberFormatException
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_toValue = new BigDecimal( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.23" ) ); //$NON-NLS-1$
      m_toValue = null;
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
      status = StatusUtilities.statusFromThrowable( t, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.24" ) + DATEFORMAT.format( new Date() ) ); //$NON-NLS-1$

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
  @Override
  public String getDomainComponentUrn( )
  {
    return m_domainComponentUrn;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getValueComponentUrn()
   */
  @Override
  public String getValueComponentUrn( )
  {
    return m_valueComponentUrn;
  }

}
