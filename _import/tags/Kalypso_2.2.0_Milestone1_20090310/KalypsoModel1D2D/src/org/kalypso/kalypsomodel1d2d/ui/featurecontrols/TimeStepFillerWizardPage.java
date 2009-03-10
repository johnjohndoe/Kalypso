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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

/**
 * @author Madanagopal
 */
public class TimeStepFillerWizardPage extends WizardPage implements SelectionListener
{
  private Text m_dateTimeFrom;

  private Text m_dateTimeTo;

  private Text m_dateTimeStep;

  private int timeStep_val;

  private Date m_dateFrom = new Date();

  private Date m_dateTo = new Date();

  protected Double m_uRelFactor;

  private static final DateFormat DATETIMEFORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm" ); //$NON-NLS-1$

  private static final DateFormat DATEFORMAT = new SimpleDateFormat( "dd.MM.yyyy 00:00" ); //$NON-NLS-1$

  private static final String DEFAULTSTEP = "60"; //$NON-NLS-1$

  private Combo m_uRelFactorCombo;

  public TimeStepFillerWizardPage( )
  {
    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.3") ); //$NON-NLS-1$
    setTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.4") ); //$NON-NLS-1$
    setDescription( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.5") ); //$NON-NLS-1$
    // TODO Auto-generated constructor stub
  }

  public TimeStepFillerWizardPage( final Date startDate, final Date endDate )
  {
    this();
    m_dateFrom = startDate;
    m_dateTo = endDate;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final GridData gridBeginning = new GridData( SWT.BEGINNING, SWT.CENTER, false, false );
    final GridData gridEnd = new GridData( SWT.END, SWT.CENTER, false, false );
    final GridData gridFillHorizontal = new GridData( SWT.FILL, SWT.CENTER, true, false );

    final Label vonLbl = new Label( container, SWT.NONE );
    vonLbl.setLayoutData( gridBeginning );
    vonLbl.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.6") ); //$NON-NLS-1$

    m_dateTimeFrom = new Text( container, SWT.BORDER );
    m_dateTimeFrom.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          // TODO: check for right time zone
          m_dateFrom = DATETIMEFORMAT.parse( m_dateTimeFrom.getText() );
          if( getStartDate().after( getFinishDate() ) )
          {
            setMessage( null );
            setErrorMessage( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.7") ); //$NON-NLS-1$
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
    // TODO: check for right time zone
    m_dateTimeFrom.setText( DATEFORMAT.format( m_dateFrom ) );
    m_dateTimeFrom.setLayoutData( gridFillHorizontal );

    final Button dateFromBtn = new Button( container, SWT.NONE );
    dateFromBtn.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.8") ); //$NON-NLS-1$
    // dateFromBtn.setEnabled( false );
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
          // TODO: check for right time zone
          m_dateTimeFrom.setText( DATETIMEFORMAT.format( m_dateFrom ) );
        }
      }
    } );

    final Label bisLbl = new Label( container, SWT.NONE );
    bisLbl.setLayoutData( gridBeginning );
    bisLbl.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.9") ); //$NON-NLS-1$

    m_dateTimeTo = new Text( container, SWT.BORDER );
    m_dateTimeTo.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        try
        {
          // TODO: check for right time zone
          m_dateTo = DATETIMEFORMAT.parse( m_dateTimeTo.getText() );
          if( getStartDate().after( getFinishDate() ) )
          {
            setMessage( null );
            setErrorMessage( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.10") ); //$NON-NLS-1$
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

    // TODO: check for right time zone
    m_dateTimeTo.setText( DATETIMEFORMAT.format( m_dateTo ) );
    m_dateTimeTo.setLayoutData( gridFillHorizontal );

    final Button dateToBtn = new Button( container, SWT.NONE );
    dateToBtn.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.11") ); //$NON-NLS-1$
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
          // TODO: check for right time zone
          m_dateTimeTo.setText( DATETIMEFORMAT.format( m_dateTo ) );
        }
      }
    } );

    final Label timeStepLbl = new Label( container, SWT.NONE );
    timeStepLbl.setLayoutData( gridBeginning );
    timeStepLbl.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.12") ); //$NON-NLS-1$

    m_dateTimeStep = new Text( container, SWT.BORDER );
    m_dateTimeStep.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String numberPattern = "\\d+"; //$NON-NLS-1$
        if( !m_dateTimeStep.getText().matches( numberPattern ) )
        {
          setMessage( null );
          setErrorMessage( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.14") ); //$NON-NLS-1$
          setPageComplete( false );
        }
        else
        {
          timeStep_val = Integer.parseInt( m_dateTimeStep.getText() );
          setMessage( null );
          setErrorMessage( null );
          setPageComplete( true );
        }
      }
    } );

    m_dateTimeStep.setText( DEFAULTSTEP );
    m_dateTimeStep.setLayoutData( gridFillHorizontal );

    final Label emptylabel_1 = new Label( container, SWT.NONE );
    emptylabel_1.setLayoutData( gridEnd );
    emptylabel_1.setText( "" ); //$NON-NLS-1$

    final Label uRelFactorLabel = new Label( container, SWT.NONE );
    uRelFactorLabel.setLayoutData( gridBeginning );
    uRelFactorLabel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.16") ); //$NON-NLS-1$

    m_uRelFactorCombo = new Combo( container, SWT.DROP_DOWN | SWT.READ_ONLY );
    // TODO: do not use Combo; use ComvoViewer instead!
    final String possibleURFValues[] = { "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
    m_uRelFactorCombo.setItems( possibleURFValues );
    m_uRelFactorCombo.select( 9 );
    m_uRelFactor = 1.0;
    m_uRelFactorCombo.setLayoutData( gridFillHorizontal );
    m_uRelFactorCombo.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String comboPattern = "0.1|0.2|0.3|0.4|0.5|0.6|0.7|0.8|0.9|1.0"; //$NON-NLS-1$
        if( !m_uRelFactorCombo.getText().matches( comboPattern ) )
        {
          setMessage( null );
          setErrorMessage( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizardPage.28") ); //$NON-NLS-1$
          setPageComplete( false );
        }
        else
        {
          m_uRelFactor = Double.parseDouble( m_uRelFactorCombo.getText() );
          setMessage( null );
          setErrorMessage( null );
          setPageComplete( true );
        }
      }
    } );
    final Label emptylabel_2 = new Label( container, SWT.NONE );
    emptylabel_2.setLayoutData( gridEnd );
    emptylabel_2.setText( "" ); //$NON-NLS-1$
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
    return timeStep_val;
  }

  public double getUnderRelaxationFactorValue( )
  {
    return m_uRelFactor;
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetDefaultSelected( final SelectionEvent e )
  {
    //
  }

  /**
   * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
   */
  public void widgetSelected( final SelectionEvent e )
  {
    final Widget w = e.widget;
    if( w instanceof Combo )
    {
      m_uRelFactor = Double.parseDouble( m_uRelFactorCombo.getText() );
    }

  }

}
