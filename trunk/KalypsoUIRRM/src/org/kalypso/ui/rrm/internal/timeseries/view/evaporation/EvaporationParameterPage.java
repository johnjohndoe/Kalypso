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
package org.kalypso.ui.rrm.internal.timeseries.view.evaporation;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.MultiValidator;
import org.kalypso.commons.databinding.validation.StringAsDoubleValidator;
import org.kalypso.commons.databinding.validation.StringFilenameValidator;
import org.kalypso.commons.databinding.validation.StringIsAsciiPrintableValidator;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.evaporation.CalculateEvaporationData.EVAPORATION_TYPE;

/**
 * @author Dirk Kuch
 */
public class EvaporationParameterPage extends WizardPage implements IUpdateable
{

  private final CalculateEvaporationData m_data;

  private DatabindingWizardPage m_binding;

  private Composite m_body;

  private Composite m_page;

  private final IStation m_station;

  protected EvaporationParameterPage( final IStation station, final CalculateEvaporationData data )
  {
    super( Messages.getString( "EvaporationParameterPage_0" ) ); //$NON-NLS-1$

    m_station = station;
    m_data = data;

    setTitle( Messages.getString( "EvaporationParameterPage_1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "EvaporationParameterPage_2" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );

    m_body = new Composite( parent, SWT.NULL );
    m_body.setLayout( Layouts.createGridLayout() );
    setControl( m_body );
  }

  @Override
  public void update( )
  {
    if( m_page != null && !m_page.isDisposed() )
      m_page.dispose();

    m_binding = new DatabindingWizardPage( this, null );

    m_page = new Composite( m_body, SWT.NULL );
    m_page.setLayout( Layouts.createGridLayout() );
    m_page.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final EVAPORATION_TYPE type = m_data.getEvaporationType();
    if( EVAPORATION_TYPE.eLandBased.equals( type ) )
      renderLandBased( m_page );
    else if( EVAPORATION_TYPE.eWaterBase.equals( type ) )
      renderWaterBased( m_page );

    m_page.layout();
    m_body.layout();

  }

  private void renderLandBased( final Composite page )
  {
    doAddLandBasedParameters( page );
    doAddQualityControl( page );
  }

  private void renderWaterBased( final Composite page )
  {
    final Group group = new Group( page, SWT.NULL );
    group.setLayout( new GridLayout( 3, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "EvaporationParameterPage_3" ) ); //$NON-NLS-1$

    addTextControl( group, Messages.getString( "EvaporationParameterPage_4" ), CalculateEvaporationData.PROPERTY_LATITUDE, new StringAsDoubleValidator( IStatus.ERROR, Messages.getString( "EvaporationParameterPage_5" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
// addTextControl( group, "Faktor Umrechnung j/cm² in W/m²", CalculateEvaporationData.PROPERTY_FACTOR_CONVERSION_JW, new
// StringAsDoubleValidator( IStatus.ERROR, "Umrechnungsfaktor ist kein gültiger Zahlenwert." ) );
// addTextControl( group, "Emissionskoeffizient", CalculateEvaporationData.PROPERTY_COEFFICIENT_EMISSION, new
// StringAsDoubleValidator( IStatus.ERROR, "Emissionskoeffizient ist kein gültiger Zahlenwert." ) );
// addTextControl( group, "Boltzmann Konstante (Wasser)", CalculateEvaporationData.PROPERTY_BOLTZMANN_WATER_CONSTANT,
// new StringAsDoubleValidator( IStatus.ERROR, "Boltzmann Konstante ist kein gültiger Zahlenwert." ) );
// addTextControl( group, "Albedo Wasserfläche", CalculateEvaporationData.PROPERTY_ALBEDO_WATER, new
// StringAsDoubleValidator( IStatus.ERROR, "Albedo ist kein gültiger Zahlenwert." ) );

    doAddQualityControl( page );
  }

  private void doAddLandBasedParameters( final Composite page )
  {
    final Group group = new Group( page, SWT.NULL );
    group.setLayout( new GridLayout( 3, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.getString( "EvaporationParameterPage_6" ) ); //$NON-NLS-1$

    addTextControl( group, Messages.getString( "EvaporationParameterPage_7" ), CalculateEvaporationData.PROPERTY_LATITUDE, new StringAsDoubleValidator( IStatus.ERROR, Messages.getString( "EvaporationParameterPage_8" ) ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private void addTextControl( final Composite parent, final String label, final String property, final IValidator validator )
  {
    final Label lab = new Label( parent, SWT.NULL );
    lab.setText( label );

    final Label spacer = new Label( parent, SWT.NULL );
    spacer.setText( "" ); //$NON-NLS-1$
    final GridData data = new GridData( GridData.FILL, GridData.FILL, false, false );
    data.widthHint = data.minimumWidth = 100;
    spacer.setLayoutData( data );

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( text, new int[] { SWT.Modify } );
    final IObservableValue model = BeansObservables.observeValue( m_data, property );

    m_binding.bindValue( target, model, validator );
  }

  private void doAddQualityControl( final Composite page )
  {
    final Group groupDateRange = new Group( page, SWT.NULL );
    groupDateRange.setLayout( new GridLayout() );
    groupDateRange.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    groupDateRange.setText( Messages.getString( "EvaporationParameterPage_10" ) ); //$NON-NLS-1$

    final Text text = new Text( groupDateRange, SWT.BORDER );
    text.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final ISWTObservableValue target = SWTObservables.observeText( text, new int[] { SWT.Modify } );
    final IObservableValue model = BeansObservables.observeValue( m_data, CalculateEvaporationData.PROPERTY_QUALITY );

    final StringIsAsciiPrintableValidator ascii = new StringIsAsciiPrintableValidator( IStatus.ERROR, Messages.getString( "EvaporationParameterPage_11" ) );
    final StringFilenameValidator filename = new StringFilenameValidator( IStatus.ERROR, Messages.getString( "EvaporationParameterPage_11" ) );
    final TimeSeriesAlreadyExistsValidator exists = new TimeSeriesAlreadyExistsValidator( m_station, m_data );

    m_binding.bindValue( target, model, new MultiValidator( ascii, filename, exists ) ); //$NON-NLS-1$
  }

}
