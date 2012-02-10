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

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

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
import org.eclipse.swt.events.SelectionListener;
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
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * Constructs a simple boundary for wave parameters.
 * 
 * @author ig
 */
public class WaveStepDescriptor implements IBoundaryConditionDescriptor
{
  public static final String DEFAULT_VALUE = "0.2 4.5 0. 2."; //$NON-NLS-1$ CON PAR 

  public static final String SWAN_BC_DEFAULT_STEADY_VALUE_PREFIX = "CON PAR "; //$NON-NLS-1$

  protected static final DateFormat DATETIMEFORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm" ); //$NON-NLS-1$

  protected static final DateFormat DATEFORMAT = new SimpleDateFormat( "dd.MM.yyyy 00:00" ); //$NON-NLS-1$

  private static final String MSG_PAGE = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.0" ); //$NON-NLS-1$

  private static final String DEFAULTSTEP = "10"; //$NON-NLS-1$
  
  private static final double DEFAULT_BOUNDARY_HSIG = 0.5;
  
  private static final double DEFAULT_BOUNDARY_PER = 4.5;

  private static final double DEFAULT_BOUNDARY_DIR = 180;
  
  private static final double DEFAULT_BOUNDARY_DD = 2.0;

  protected final Date[] m_dates = new Date[2];

  private WizardPage m_page;

  private final String m_hsigComponentUrn;

  private final String m_domainComponentUrn;

  private final String m_name;

  private final Double[] m_arrayDoubleHSig = new Double[2];

  private final Double[] m_arrayDoublePer = new Double[2];
  
  private final Double[] m_arrayDoubleDD = new Double[2];
  
  private final Double[] m_arrayDoubleDir = new Double[2];
  
  private int m_field = Calendar.HOUR;
  
  private Integer m_step;

  private boolean m_stateAbsoluteCond = false;


  public WaveStepDescriptor( final String name )
  {
//    m_name = "Wave Boundary Parameters";//name;
    m_name = name;

    m_domainComponentUrn = Kalypso1D2DDictConstants.DICT_COMPONENT_TIME;
    m_hsigComponentUrn = Kalypso1D2DDictConstants.DICT_COMPONENT_WAVE_HSIG;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.jface.wizard.WizardPage)
   */
  public Control createControl( final Composite parent, final WizardPage page )
  {
    m_page = page;

    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout( 5, false );
    container.setLayout( gridLayout );
    
    createDateEntryLine( container, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.5"), 0, 0 ); //$NON-NLS-1$
    createDateEntryLine( container, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.6"), 1, 24 ); //$NON-NLS-1$

    final Label stepLabel = new Label( container, SWT.NONE );
    stepLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    stepLabel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.7") ); //$NON-NLS-1$

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
    
    new Label( container, SWT.NONE ).setText( "" ); //$NON-NLS-1$;
    new Label( container, SWT.NONE ).setText( "" ); //$NON-NLS-1$;
    new Label( container, SWT.NONE ).setText( "" ); //$NON-NLS-1$;
    
    new Label( container, SWT.NONE ).setText( "hSig [m]" ); 
    new Label( container, SWT.NONE ).setText( "Per [s]" ); 
    new Label( container, SWT.NONE ).setText( "Direction" ); 
    new Label( container, SWT.NONE ).setText( "DD" ); 
  
    createFromToLineWaveParams( container, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.4"), 0, new double[]{ 0.0, 0.0, 0.0, DEFAULT_BOUNDARY_DD } ); //$NON-NLS-1$, 0.0, 0.0, m_arrayDoubleHSig
    createFromToLineWaveParams( container, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.5"), 1, new double[]{ DEFAULT_BOUNDARY_HSIG, DEFAULT_BOUNDARY_PER, DEFAULT_BOUNDARY_DIR, DEFAULT_BOUNDARY_DD } ); //$NON-NLS-1$, 0.0, 3.5, m_arrayDoublePer

    new Label( container, SWT.NONE ).setText( "" ); //$NON-NLS-1$
    
    final Label constBCLabel = new Label( container, SWT.NONE );
    constBCLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );
    constBCLabel.setText( "Constant boundary condition only" ); 

    Button m_checkbox = new Button( container, SWT.CHECK );
    m_checkbox.setToolTipText( "" );
    m_checkbox.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        setBoundaryAbsoluteChanged();
      }

      @Override
      public void widgetDefaultSelected( SelectionEvent e )
      {
      }

    } );


    updatePageState( Status.OK_STATUS );

    return container;
  }
  
  protected void setBoundaryAbsoluteChanged( )
  {
    m_stateAbsoluteCond = !m_stateAbsoluteCond ;
    
  }

  private void createFromToLineWaveParams( final Composite container, final String pStrLabel, final int pIntPosInArr, final double[] pArrInitValues )
  {
    final Label labelLabel = new Label( container, SWT.NONE );
    labelLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    labelLabel.setText( pStrLabel );

    final Text textHSig = new Text( container, SWT.BORDER );
    textHSig.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    textHSig.addModifyListener( createModifyListener( m_arrayDoubleHSig, textHSig, pIntPosInArr) ); 
    textHSig.setText( String.format( "%s", pArrInitValues[ 0 ] ) ); //$NON-NLS-1$

    final Text textPer = new Text( container, SWT.BORDER );
    textPer.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    textPer.addModifyListener( createModifyListener( m_arrayDoublePer, textPer, pIntPosInArr ) ); 
    textPer.setText( String.format( "%s", pArrInitValues[ 1 ] ) ); //$NON-NLS-1$
    
    final Text textDir = new Text( container, SWT.BORDER );
    textDir.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    textDir.addModifyListener( createModifyListener( m_arrayDoubleDir, textDir, pIntPosInArr ) ); 
    textDir.setText( String.format( "%s", pArrInitValues[ 2 ] ) ); //$NON-NLS-1$
    
    final Text textDD = new Text( container, SWT.BORDER );
    textDD.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    textDD.addModifyListener( createModifyListener( m_arrayDoubleDD, textDD, pIntPosInArr ) ); 
    textDD.setText( String.format( "%s", pArrInitValues[ 3 ] ) ); //$NON-NLS-1$

  }

  private ModifyListener createModifyListener( final Double[] arrayDouble, final Text textH, final int i )
  {
    return new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = textH.getText();
        handleValueChanged( i, text, arrayDouble );
      }
    };
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
    
    new Label( parent, SWT.NONE ).setText( "" ); //$NON-NLS-1$;
    new Label( parent, SWT.NONE ).setText( "" ); //$NON-NLS-1$;

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
    if( m_arrayDoubleHSig[0] == null )
      return StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.9") ); //$NON-NLS-1$
    if( m_arrayDoubleHSig[1] == null )
      return StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.10") ); //$NON-NLS-1$
    if( m_arrayDoubleDir[0] == null )
      return StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.11") ); //$NON-NLS-1$
    if( m_arrayDoubleDir[1] == null )
      return StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.12") ); //$NON-NLS-1$
    if( m_step == null )
      return StatusUtilities.createWarningStatus( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.13") ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }
  
  protected void handleFieldChanged( final int field )
  {
    m_field = field;

    updatePageState( Status.OK_STATUS );
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#activate()
   */
  public void activate( )
  {
    m_page.setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.15" ) );
    m_page.setDescription( MSG_PAGE );

    updatePageState( Status.OK_STATUS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#initializeBC(org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition)
   */
  public void initializeBC( final IBoundaryCondition bc )
  {
    bc.setName( getName() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#fillObservation(org.kalypso.observation.IObservation)
   */
  @SuppressWarnings("deprecation")
  public void fillObservation( final IObservation<TupleResult> obs ) throws InvocationTargetException
  {
    try
    {
      final TupleResult result = obs.getResult();

      obs.setName( getName() );
      
      obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) ); //$NON-NLS-1$

      final IComponent[] components = result.getComponents();
      final IComponent domainComponent = components[ 0 ];
      final IComponent valueComponent1 = components[ 1 ];
      final IComponent valueComponent2 = components[ 2 ];
      final IComponent valueComponent3 = components[ 3 ];
      final IComponent valueComponent4 = components[ 4 ];
      
      result.setSortComponents( new IComponent[] { domainComponent } );

      final Double lDoubleHsigStart = m_arrayDoubleHSig[0];
      final Double lDoubleHsigStop = m_arrayDoubleHSig[1];
      final Double lDoubleDirStart = m_arrayDoubleDir[0];
      final Double lDoubleDirStop = m_arrayDoubleDir[1];
      final Double lDoublePerStart = m_arrayDoublePer[0];
      final Double lDoublePerStop = m_arrayDoublePer[1];
      final Double lDoubleDDStart = m_arrayDoubleDD[0];
      final Double lDoubleDDStop = m_arrayDoubleDD[1];

      final GregorianCalendar calendarFrom = new GregorianCalendar();
      final GregorianCalendar calendarTo = new GregorianCalendar();
      
      calendarFrom.setTime( m_dates[0] );
      calendarTo.setTime( m_dates[1] );

      while( !calendarFrom.after( calendarTo ) )
      {
        final IRecord record = result.createRecord();
        record.setValue( domainComponent, new XMLGregorianCalendarImpl( calendarFrom ) );

        final BigDecimal valueHsig = interpolateValue( calendarFrom, lDoubleHsigStart, lDoubleHsigStop );
        final BigDecimal valueDir = interpolateValue( calendarFrom, lDoubleDirStart, lDoubleDirStop );
        final BigDecimal valuePer = interpolateValue( calendarFrom, lDoublePerStart, lDoublePerStop );
        final BigDecimal valueDD = interpolateValue( calendarFrom, lDoubleDDStart, lDoubleDDStop );

        if( valueHsig != null )
        {
          record.setValue( valueComponent1, valueHsig );
        }
        
        if( valuePer != null )
        {
          record.setValue( valueComponent2, valuePer );
        }

        if( valueDir != null )
        {
          record.setValue( valueComponent3, valueDir );
        }

        if( valueDD != null )
        {
          record.setValue( valueComponent4, valueDD );
        }
        result.add( record );
        
        calendarFrom.add( m_field, m_step );
      }
    }
    catch( final Throwable t )
    {
      throw new InvocationTargetException( t );
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
      m_step = new Integer( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.18") ); //$NON-NLS-1$
      m_step = null;
    }

    updatePageState( status );
  }

  protected void handleValueChanged( final int index, final String text, Double[] pArrayDoubleTarget )
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      pArrayDoubleTarget[ index ] = new Double( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WaveStepDescriptor.18" ) );//"org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.19") ); //$NON-NLS-1$

      pArrayDoubleTarget[ index ] = null;//-9999.0;
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
      status = StatusUtilities.statusFromThrowable( t, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor.24") + DATEFORMAT.format( new Date() ) ); //$NON-NLS-1$

      m_dates[index] = null;
    }

    updatePageState( status );
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
    return m_hsigComponentUrn;
  }
  
  protected BigDecimal interpolateValue( final GregorianCalendar calendar, Double pDoubleStartValue, Double pDoubleStopValue )
  {
    
    /* Linear interpolation */
    final XMLGregorianCalendar from = DateUtilities.toXMLGregorianCalendar( m_dates[0] );
    final XMLGregorianCalendar to = DateUtilities.toXMLGregorianCalendar( m_dates[1] );

    final long before = from.toGregorianCalendar().getTimeInMillis();
    final long after = to.toGregorianCalendar().getTimeInMillis();

    final long dom = calendar.getTimeInMillis();

    final double valBefore = ((Number) pDoubleStartValue).doubleValue();
    final double valAfter = ((Number) pDoubleStopValue).doubleValue();

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

  public final boolean isStateAbsoluteCond( )
  {
    return m_stateAbsoluteCond;
  }
  
}
