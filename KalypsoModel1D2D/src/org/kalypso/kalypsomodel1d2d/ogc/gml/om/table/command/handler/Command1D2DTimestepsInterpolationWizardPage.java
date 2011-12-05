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
package org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.INTERPOLATION_METHOD;

/**
 * @author Dejan Antanaskovic
 */
public class Command1D2DTimestepsInterpolationWizardPage extends WizardPage
{
//changed to string to allow more flexible expansion of "Relaxation Factor"
  private Text m_uRelFactorCombo;

  private Command1D2DTimestepsInterpolate.INTERPOLATION_METHOD m_interpolationMethod = INTERPOLATION_METHOD.TIME_INTERVAL;

  private Label m_dateTimeLbl;

  private Label m_numberOfStepsLbl;

  private Spinner m_numberOfStepsFld;

  private Spinner m_dateFld;

  private DateTime m_timeFld;

  private final String m_initRelaxationFactor;

  public Command1D2DTimestepsInterpolationWizardPage( final String initRelaxationFactor )
  {
    super( "Example" ); //$NON-NLS-1$
    setTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.1") ); //$NON-NLS-1$
    setDescription( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.2") ); //$NON-NLS-1$
    m_initRelaxationFactor = initRelaxationFactor;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final GridLayout gridLayout1col = new GridLayout();
    gridLayout1col.numColumns = 1;
    final GridLayout gridLayout3col = new GridLayout();
    gridLayout3col.numColumns = 3;
    final GridLayout gridLayout3colNoMargin = new GridLayout();
    gridLayout3colNoMargin.numColumns = 3;
    gridLayout3colNoMargin.marginWidth = 0;
    gridLayout3colNoMargin.marginHeight = 0;

    final Composite topContainer = new Composite( parent, SWT.NULL );
    topContainer.setLayout( gridLayout1col );
    setControl( topContainer );

    final Group group = new Group( topContainer, SWT.SHADOW_NONE );
    group.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.3") ); //$NON-NLS-1$
    group.setLayout( gridLayout1col );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    final Button option1 = new Button( group, SWT.RADIO );
    option1.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.4") ); //$NON-NLS-1$
    option1.setSelection( true );
    option1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    option1.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        setInterpolationMethod( INTERPOLATION_METHOD.TIME_INTERVAL );
      }
    } );
    final Button option2 = new Button( group, SWT.RADIO );
    option2.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.5") ); //$NON-NLS-1$
    option2.setSelection( false );
    option2.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
    option2.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        setInterpolationMethod( INTERPOLATION_METHOD.NUMBER_OF_STEPS );
      }
    } );

    final Composite container = new Composite( topContainer, SWT.NULL );
    container.setLayout( gridLayout3col );
    container.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_dateTimeLbl = new Label( container, SWT.NONE );
    m_dateTimeLbl.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    m_dateTimeLbl.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.6") ); //$NON-NLS-1$

    final Composite dateTimeContainer = new Composite( container, SWT.NULL );
    dateTimeContainer.setLayout( gridLayout3colNoMargin );
    dateTimeContainer.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_dateFld = new Spinner( dateTimeContainer, SWT.NONE );
    m_dateFld.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    m_dateFld.setMinimum( 0 );
    m_dateFld.setMaximum( 365 );
    m_dateFld.setIncrement( 1 );
    m_dateFld.setSelection( 0 );

    m_timeFld = new DateTime( dateTimeContainer, SWT.TIME );
    m_timeFld.setHours( 0 );
    m_timeFld.setMinutes( 0 );
    m_timeFld.setSeconds( 0 );
    m_timeFld.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_dateFld.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        updateInterface();
      }
    } );
    m_timeFld.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        updateInterface();
      }
    } );

    final Label endLbl1 = new Label( dateTimeContainer, SWT.NONE );
    endLbl1.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    final Label endLbl2 = new Label( container, SWT.NONE );
    endLbl2.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    m_numberOfStepsLbl = new Label( container, SWT.NONE );
    m_numberOfStepsLbl.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    m_numberOfStepsLbl.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.7") ); //$NON-NLS-1$

    m_numberOfStepsFld = new Spinner( container, SWT.NONE );
    m_numberOfStepsFld.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_numberOfStepsFld.setMinimum( 2 );
    m_numberOfStepsFld.setMaximum( 100000 );
    m_numberOfStepsFld.setIncrement( 1 );
    m_numberOfStepsFld.setSelection( 2 );
    m_timeFld.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        updateInterface();
      }
    } );
    final Label endLbl3 = new Label( container, SWT.NONE );
    endLbl3.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    final Label uRelFactorLabel = new Label( container, SWT.NONE );
    uRelFactorLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    uRelFactorLabel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizardPage.8") ); //$NON-NLS-1$

//  changed to string to allow more flexible expansion of "Relaxation Factor"	
    m_uRelFactorCombo = new Text( container, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_uRelFactorCombo.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    /*
    m_uRelFactorCombo = new Combo( container, SWT.DROP_DOWN | SWT.READ_ONLY );
    final String possibleURFValues[] = { "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$
    m_uRelFactorCombo.setItems( possibleURFValues );
    int cmbIndex = 0;
    for( ; cmbIndex < possibleURFValues.length; cmbIndex++ )
      if( possibleURFValues[cmbIndex].equals( m_initRelaxationFactor ) )
        break;
    m_uRelFactorCombo.select( cmbIndex );
    m_uRelFactorCombo.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    */
    final Label endLbl4 = new Label( container, SWT.NONE );
    endLbl4.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );

    topContainer.layout();
    updateInterface();
  }

  protected void setInterpolationMethod( final INTERPOLATION_METHOD method )
  {
    m_interpolationMethod = method;
    updateInterface();
  }

  protected void updateInterface( )
  {
    final boolean isComplete;
    switch( m_interpolationMethod )
    {
      case NUMBER_OF_STEPS:
        isComplete = m_numberOfStepsFld.getSelection() >= 2;
        m_numberOfStepsLbl.setEnabled( true );
        m_numberOfStepsFld.setEnabled( true );
        m_dateTimeLbl.setEnabled( false );
        m_dateFld.setEnabled( false );
        m_timeFld.setEnabled( false );
        break;
      case TIME_INTERVAL:
      default:
        isComplete = m_dateFld.getSelection() != 0 || m_timeFld.getHours() != 0 || m_timeFld.getMinutes() != 0 || m_timeFld.getSeconds() != 0;
        m_numberOfStepsLbl.setEnabled( false );
        m_numberOfStepsFld.setEnabled( false );
        m_dateTimeLbl.setEnabled( true );
        m_dateFld.setEnabled( true );
        m_timeFld.setEnabled( true );
        break;
    }
    setPageComplete( isComplete );
    getContainer().updateButtons();
  }

  public INTERPOLATION_METHOD getInterpolationMethod( )
  {
    return m_interpolationMethod;
  }

  public int getNumberOfSteps( )
  {
    return m_numberOfStepsFld.getSelection();
  }

  // in millis
  public long getTimeInterval( )
  {
    final long MILLIS_PER_DAY = 86400000;
    final long MILLIS_PER_HOUR = 3600000;
    final long MILLIS_PER_MINUTE = 60000;
    final long MILLIS_PER_SECOND = 1000;
    return MILLIS_PER_DAY * m_dateFld.getSelection() + MILLIS_PER_HOUR * m_timeFld.getHours() + MILLIS_PER_MINUTE * m_timeFld.getMinutes() + MILLIS_PER_SECOND * m_timeFld.getSeconds();
  }

  public String getRelaxationFactorValue( )
  {
    return m_uRelFactorCombo.getText();
  }

}
