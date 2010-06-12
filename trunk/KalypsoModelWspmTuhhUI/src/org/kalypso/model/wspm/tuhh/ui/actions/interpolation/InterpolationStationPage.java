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
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
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

/**
 * @author Gernot Belger
 */
public class InterpolationStationPage extends WizardPage
{
  private final IProfileFeature[] m_profiles;

  private BigDecimal m_station;

  private final NoTwoNeighbouringStationsError m_neighbourRule;

  private IProfileFeature m_prevProfile;

  private IProfileFeature m_nextProfile;

  public InterpolationStationPage( final String pageName, final IProfileFeature[] profiles )
  {
    super( pageName );
    m_profiles = profiles;

    m_neighbourRule = new NoTwoNeighbouringStationsError( m_profiles );

    setTitle( "Enter new station" );
    setDescription( "Please choose the station for which to interpolate a new profile." );
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
    group.setText( "Station" );

    // 1st line

    final Label prevLabel = new Label( group, SWT.NONE );
    prevLabel.setText( "Previous profile [km]" );
    prevLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    final Label kmLabel = new Label( group, SWT.NONE );
    kmLabel.setText( "Interpolated Station [km]" );
    kmLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    final Label nextLabel = new Label( group, SWT.NONE );
    nextLabel.setText( "Next profile [km]" );
    nextLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    // // 2nd line

    final Label prevStation = new Label( group, SWT.NONE );
    prevStation.setText( "" );
    prevStation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Text stationEditor = new Text( group, SWT.BORDER | SWT.TRAIL );
    stationEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Label nextStation = new Label( group, SWT.NONE );
    nextStation.setText( "" );
    nextStation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

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

    final ValidatingModifyListener stationValidator = new ValidatingModifyListener( stationEditor, parser, messageManager );
    stationValidator.setValidator( validator );
    stationValidator.setValueReceiver( receiver );
  }

  protected void setStation( final BigDecimal station, final Label prevLabel, final Label nextLabel )
  {
    m_station = station;

    m_prevProfile = m_neighbourRule.getPreviousProfile( station );
    m_nextProfile = m_neighbourRule.getNextProfile( station );

    if( m_prevProfile == null )
      prevLabel.setText( "Not Found" );
    else
      prevLabel.setText( m_prevProfile.getBigStation().toString() );

    if( m_nextProfile == null )
      nextLabel.setText( "Not Found" );
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
}
