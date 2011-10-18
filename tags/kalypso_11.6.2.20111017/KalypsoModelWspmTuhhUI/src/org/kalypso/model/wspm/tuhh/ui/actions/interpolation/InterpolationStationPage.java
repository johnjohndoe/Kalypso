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

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.WidgetProperties;
import org.eclipse.jface.databinding.wizard.WizardPageSupport;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.ManagedForm;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class InterpolationStationPage extends WizardPage
{
  private final DataBindingContext m_bindingContext = new DataBindingContext();

  private ManagedForm m_form;

  private Label m_prevStation;

  private Label m_nextStation;

  private final InterpolationStationData m_interpolationData;

  private WizardPageSupport m_pageSupport;

  public InterpolationStationPage( final String pageName, final InterpolationStationData interpolationData )
  {
    super( pageName );

    m_interpolationData = interpolationData;

    setTitle( Messages.getString( "InterpolationStationPage_1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "InterpolationStationPage_2" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.DialogPage#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_pageSupport != null )
    {
      m_pageSupport.dispose();
    }

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    m_form = new ManagedForm( parent );
    final ScrolledForm form = m_form.getForm();

    setControl( form );

    final Composite body = form.getBody();
    body.setLayout( new FillLayout() );

    final Group group = new Group( body, SWT.NONE );
    final GridLayout groupLayout = new GridLayout( 3, true );
    groupLayout.horizontalSpacing = 10;
    group.setLayout( groupLayout );
    group.setText( Messages.getString( "InterpolationStationPage_3" ) ); //$NON-NLS-1$

    // 1st line
    final Label prevLabel = new Label( group, SWT.NONE );
    prevLabel.setText( Messages.getString( "InterpolationStationPage_4" ) ); //$NON-NLS-1$
    prevLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    final Label kmLabel = new Label( group, SWT.NONE );
    kmLabel.setText( Messages.getString( "InterpolationStationPage_5" ) ); //$NON-NLS-1$
    kmLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    final Label nextLabel = new Label( group, SWT.NONE );
    nextLabel.setText( Messages.getString( "InterpolationStationPage_6" ) ); //$NON-NLS-1$
    nextLabel.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );

    // // 2nd line
    m_prevStation = new Label( group, SWT.NONE );
    m_prevStation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Text stationEditor = new Text( group, SWT.BORDER | SWT.TRAIL );
    stationEditor.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_nextStation = new Label( group, SWT.NONE );
    m_nextStation.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    // / 3rd line
    final Button onlyChannelCheck = new Button( group, SWT.CHECK );
    onlyChannelCheck.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 1 ) );
    onlyChannelCheck.setText( Messages.getString( "InterpolationStationPage_7" ) ); //$NON-NLS-1$

    createBinding( stationEditor, onlyChannelCheck );
  }

  protected void createBinding( final Text stationEditor, final Button onlyChannelCheck )
  {
    m_pageSupport = WizardPageSupport.create( this, m_bindingContext );

    bindOnlyChannel( onlyChannelCheck );
    bindStation( stationEditor );
  }

  private void bindOnlyChannel( final Button onlyChannelCheck )
  {
    final ISWTObservableValue checkValue = WidgetProperties.selection().observe( onlyChannelCheck );
    m_bindingContext.bindValue( checkValue, m_interpolationData.observeOnlyChannel() );
  }

  private void bindStation( final Text stationEditor )
  {
    final UpdateValueStrategy update = new UpdateValueStrategy();
    final IProfileFeature[] profiles = m_interpolationData.getProfiles();
    update.setAfterConvertValidator( new NoTwoNeighbouringStationsError( profiles ) );

    final IObservableValue stationTextValue = WidgetProperties.text( SWT.Modify ).observe( stationEditor );
    m_bindingContext.bindValue( stationTextValue, m_interpolationData.observeStation(), update, null );

    m_bindingContext.bindValue( WidgetProperties.text().observe( m_prevStation ), m_interpolationData.observePrevLabel(), null, null );
    m_bindingContext.bindValue( WidgetProperties.text().observe( m_nextStation ), m_interpolationData.observeNextLabel(), null, null );
  }
}
