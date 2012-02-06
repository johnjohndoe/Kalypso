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
package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author Madanagopal
 */
public class TimeStepFillerWizardPage extends WizardPage
{
  int m_timeStep_val;

  Date m_dateFrom = new Date();

  Date m_dateTo = new Date();

  String m_uRelFactor;

  int m_field = Calendar.MINUTE;

  public TimeStepFillerWizardPage( )
  {
    this( new Date(), new Date(), new BigDecimal( "1.0" ), 60 ); //$NON-NLS-1$
  }

  public TimeStepFillerWizardPage( final Date startDate, final Date endDate, final BigDecimal uRelFactor, final int timeStep )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.3" ) ); //$NON-NLS-1$

    setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.4" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.5" ) ); //$NON-NLS-1$

    m_uRelFactor = "" + uRelFactor;// uRelFactor.setScale( 1, BigDecimal.ROUND_HALF_UP ); //$NON-NLS-1$
    m_timeStep_val = timeStep;
    m_dateFrom = startDate;
    m_dateTo = endDate;
  }

  public TimeStepFillerWizardPage( final Date startDate, final Date endDate, final String uRelFactor, final int timeStep )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.3" ) ); //$NON-NLS-1$

    setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.4" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.5" ) ); //$NON-NLS-1$

    m_uRelFactor = uRelFactor;
    m_timeStep_val = timeStep;
    m_dateFrom = startDate;
    m_dateTo = endDate;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final TimeZone timeZone = KalypsoCorePlugin.getDefault().getTimeZone();
    final DateFormat dateTimeFormat = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss.SSS" ); //$NON-NLS-1$
    dateTimeFormat.setTimeZone( timeZone );

    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout( 3, false );
    container.setLayout( gridLayout );
    setControl( container );

    final GridData gridBeginning = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    final GridData gridFillHorizontal = new GridData( SWT.FILL, SWT.CENTER, true, false );

    final Label vonLbl = new Label( container, SWT.NONE );
    vonLbl.setLayoutData( gridBeginning );
    vonLbl.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.6" ) ); //$NON-NLS-1$

    final Text dateTimeFrom = new Text( container, SWT.BORDER );
    dateTimeFrom.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          m_dateFrom = dateTimeFormat.parse( dateTimeFrom.getText() );
          if( getStartDate().after( getFinishDate() ) )
          {
            setMessage( null );
            setErrorMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.7" ) ); //$NON-NLS-1$
            setPageComplete( false );
          }
          else
          {
            setMessage( null );
            setErrorMessage( null );
            setPageComplete( true );
          }

        }
        catch( final ParseException e1 )
        {
          // m_diagView.removeAllItems();
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
      }
    } );
    dateTimeFrom.setText( dateTimeFormat.format( m_dateFrom ) );
    dateTimeFrom.setLayoutData( gridFillHorizontal );

    final Button dateFromBtn = new Button( container, SWT.NONE );
    dateFromBtn.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.8" ) ); //$NON-NLS-1$
    dateFromBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( getShell(), m_dateFrom );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateFrom = calendarDialog.getDate();
          dateTimeFrom.setText( dateTimeFormat.format( m_dateFrom ) );
        }
      }
    } );

    final Label bisLbl = new Label( container, SWT.NONE );
    bisLbl.setLayoutData( gridBeginning );
    bisLbl.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.9" ) ); //$NON-NLS-1$

    final Text dateTimeTo = new Text( container, SWT.BORDER );
    dateTimeTo.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          m_dateTo = dateTimeFormat.parse( dateTimeTo.getText() );
          if( getStartDate().after( getFinishDate() ) )
          {
            setMessage( null );
            setErrorMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.10" ) ); //$NON-NLS-1$
            setPageComplete( false );
          }
          else
          {
            setMessage( null );
            setErrorMessage( null );
            setPageComplete( true );
          }
        }
        catch( final ParseException e1 )
        {
          setPageComplete( false );
        }
        getWizard().getContainer().updateButtons();
      }
    } );

    dateTimeTo.setText( dateTimeFormat.format( m_dateTo ) );
    dateTimeTo.setLayoutData( gridFillHorizontal );

    final Button dateToBtn = new Button( container, SWT.NONE );
    dateToBtn.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.11" ) ); //$NON-NLS-1$
    dateToBtn.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final SWTCalendarDialog calendarDialog = new SWTCalendarDialog( getShell(), m_dateTo );
        if( calendarDialog.open() == Window.OK )
        {
          m_dateTo = calendarDialog.getDate();
          dateTimeTo.setText( dateTimeFormat.format( m_dateTo ) );
        }
      }
    } );

    final Label timeStepLbl = new Label( container, SWT.NONE );
    timeStepLbl.setLayoutData( gridBeginning );
    timeStepLbl.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.12" ) ); //$NON-NLS-1$

    final Text dateTimeStep = new Text( container, SWT.BORDER );
    dateTimeStep.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String numberPattern = "\\d+"; //$NON-NLS-1$
        if( !dateTimeStep.getText().matches( numberPattern ) )
        {
          setMessage( null );
          setErrorMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.14" ) ); //$NON-NLS-1$
          setPageComplete( false );
        }
        else
        {
          m_timeStep_val = Integer.parseInt( dateTimeStep.getText() );
          setMessage( null );
          setErrorMessage( null );
          setPageComplete( true );
        }
      }
    } );

    dateTimeStep.setText( Integer.toString( m_timeStep_val ) );
    dateTimeStep.setLayoutData( gridFillHorizontal );

    final ComboViewer comboViewer = new ComboViewer( container, SWT.DROP_DOWN | SWT.READ_ONLY );
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
        m_field = ((FIELD) ((IStructuredSelection) event.getSelection()).getFirstElement()).getField();
        getWizard().getContainer().updateButtons();
      }
    } );
    comboViewer.setSelection( new StructuredSelection( FIELD.MINUTE ), true );

    final Label uRelFactorLabel = new Label( container, SWT.NONE );
    uRelFactorLabel.setLayoutData( gridBeginning );
    uRelFactorLabel.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.16" ) ); //$NON-NLS-1$

    // changed to string to allow more flexible expansion of "Relaxation Factor"
    final Text uRelFactorCombo = new Text( container, SWT.BORDER );

    uRelFactorCombo.setLayoutData( gridFillHorizontal );
    uRelFactorCombo.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        m_uRelFactor = uRelFactorCombo.getText();

        getWizard().getContainer().updateButtons();
      }
    } );

    container.layout();
  }

  public Date getStartDate( )
  {
    return m_dateFrom;
  }

  public Date getFinishDate( )
  {
    return m_dateTo;
  }

  public int getTimeSteps( )
  {
    return m_timeStep_val;
  }

  // changed to string to allow more flexible expansion of "Relaxation Factor"
  public String getUnderRelaxationFactorValue( )
  {
    return m_uRelFactor;
  }

  public int getField( )
  {
    return m_field;
  }
}
