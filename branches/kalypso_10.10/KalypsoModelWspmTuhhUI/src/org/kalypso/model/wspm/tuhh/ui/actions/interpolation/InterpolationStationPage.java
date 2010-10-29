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
package org.kalypso.model.wspm.tuhh.ui.actions.interpolation;

import java.math.BigDecimal;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IMessageManager;
import org.eclipse.ui.forms.ManagedForm;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.validation.BigDecimalParser;
import org.kalypso.commons.validation.IValueReceiver;
import org.kalypso.commons.validation.NotNullError;
import org.kalypso.commons.validation.RuleValidator;
import org.kalypso.commons.validation.TooMuchDecimalsIgnoredWarning;
import org.kalypso.commons.validation.ValidatingModifyListener;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class InterpolationStationPage extends WizardPage
{
  private static final String STR_NOT_FOUND = Messages.getString("InterpolationStationPage_0"); //$NON-NLS-1$

  private final IProfileFeature[] m_profiles;

  private BigDecimal m_station;

  private final NoTwoNeighbouringStationsError m_neighbourRule;

  private IProfileFeature m_prevProfile;

  private IProfileFeature m_nextProfile;

  private boolean m_onlyRiverChannel = true;

  public InterpolationStationPage( final String pageName, final IProfileFeature[] profiles )
  {
    super( pageName );
    m_profiles = profiles;

    m_neighbourRule = new NoTwoNeighbouringStationsError( m_profiles );

    setTitle( Messages.getString("InterpolationStationPage_1") ); //$NON-NLS-1$
    setDescription( Messages.getString("InterpolationStationPage_2") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final ScrolledForm form = new ScrolledForm( parent );
    form.setExpandHorizontal( true );
    form.setExpandVertical( true );
    setControl( form );

    final FormToolkit toolkit = new FormToolkit( parent.getDisplay() );
    form.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        toolkit.dispose();
      }
    } );

    final ManagedForm managedForm = new ManagedForm( toolkit, form );

    final IMessageManager messageManager = managedForm.getMessageManager();
    final Composite body = form.getBody();
    body.setLayout( new FillLayout() );

    final Group group = new Group( body, SWT.NONE );
    final GridLayout groupLayout = new GridLayout( 3, true );
    groupLayout.horizontalSpacing = 10;
    group.setLayout( groupLayout );
    group.setText( Messages.getString("InterpolationStationPage_3") ); //$NON-NLS-1$

    // 1st line

    final Label prevLabel = new Label( group, SWT.NONE );
    prevLabel.setText( Messages.getString("InterpolationStationPage_4") ); //$NON-NLS-1$
    prevLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    final Label kmLabel = new Label( group, SWT.NONE );
    kmLabel.setText( Messages.getString("InterpolationStationPage_5") ); //$NON-NLS-1$
    kmLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    final Label nextLabel = new Label( group, SWT.NONE );
    nextLabel.setText( Messages.getString("InterpolationStationPage_6") ); //$NON-NLS-1$
    nextLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    // // 2nd line

    final Label prevStation = new Label( group, SWT.NONE );
    prevStation.setText( "" ); //$NON-NLS-1$
    prevStation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Text stationEditor = new Text( group, SWT.BORDER | SWT.TRAIL );
    stationEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Label nextStation = new Label( group, SWT.NONE );
    nextStation.setText( "" ); //$NON-NLS-1$
    nextStation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    // / 3rd line

    final Button onlyChannelCheck = new Button( group, SWT.CHECK );
    onlyChannelCheck.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 1 ) );
    onlyChannelCheck.setText( Messages.getString("InterpolationStationPage_7") ); //$NON-NLS-1$
    onlyChannelCheck.setSelection( m_onlyRiverChannel );

    // / Rules

    final RuleValidator validator = new RuleValidator();
    validator.addRule( new NotNullError() );
    validator.addRule( new TooMuchDecimalsIgnoredWarning( IProfileFeature.STATION_SCALE ) );
    validator.addRule( m_neighbourRule );

    final BigDecimalParser parser = new BigDecimalParser( true, IProfileFeature.STATION_SCALE );

    final IValueReceiver receiver = new IValueReceiver()
    {
      @Override
      public void updateValue( final Object object )
      {
        final BigDecimal station = (BigDecimal) object;
        setStation( station, prevStation, nextStation );
      }
    };

    onlyChannelCheck.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleOnlyChannelRadioSelected( onlyChannelCheck.getSelection() );
      }
    } );

    final ValidatingModifyListener stationValidator = new ValidatingModifyListener( stationEditor, parser, messageManager );
    stationValidator.setValidator( validator );
    stationValidator.setValueReceiver( receiver );
  }

  protected void handleOnlyChannelRadioSelected( final boolean selection )
  {
    m_onlyRiverChannel = selection;
  }

  protected void setStation( final BigDecimal station, final Label prevLabel, final Label nextLabel )
  {
    m_station = station;

    m_prevProfile = m_neighbourRule.getPreviousProfile( station );
    m_nextProfile = m_neighbourRule.getNextProfile( station );

    if( m_prevProfile == null )
      prevLabel.setText( STR_NOT_FOUND );
    else
      prevLabel.setText( m_prevProfile.getBigStation().toString() );

    if( m_nextProfile == null )
      nextLabel.setText( STR_NOT_FOUND );
    else
      nextLabel.setText( m_nextProfile.getBigStation().toString() );

    setPageComplete( m_station != null && m_prevProfile != null && m_nextProfile != null );
  }

  public IProfileFeature getPreviousProfile( )
  {
    return m_prevProfile;
  }

  public IProfileFeature getNextProfile( )
  {
    return m_nextProfile;
  }

  public BigDecimal getNewStation( )
  {
    return m_station;
  }

  public boolean getOnlyRiverChannel( )
  {
    return m_onlyRiverChannel;
  }
}
