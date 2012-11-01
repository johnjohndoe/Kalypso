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
package org.kalypso.ui.wizards.lengthsection;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;

/**
 * Choose existing river line theme (line theme), river name field selection combo, river name selection combo, delta
 * station spinner, station field select combos (from / to). *
 *
 * @author Thomas Jung * *
 */
public class ConfigureLengthSectionWizardPage extends WizardPage
{
  private final CreateLengthSectionData m_data;

  private DatabindingWizardPage m_binding;

  public ConfigureLengthSectionWizardPage( final String title, final CreateLengthSectionData data )
  {
    super( "generateLengthSection", title, null ); //$NON-NLS-1$

    m_data = data;

    setDescription( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.3" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final int comboWidth1 = 75;
    final int comboWidth2 = 50;

    final Composite composite = new Composite( parent, SWT.NONE );
    setControl( composite );
    composite.setLayout( new GridLayout() );

    /* Status */
    final IStatus status = m_data.getStatus();
    if( !status.isOK() )
    {
      final StatusComposite statusComposite = new StatusComposite( composite, SWT.NONE );
      statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      statusComposite.setStatus( status );
    }

    /* river line selection group */
    final Group riverLineGroup = new Group( composite, SWT.NONE );
    riverLineGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverLineGroup.setLayout( new GridLayout( 2, false ) );
    riverLineGroup.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.4" ) ); //$NON-NLS-1$

    final Label riverLineText = new Label( riverLineGroup, SWT.NONE );
    riverLineText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverLineText.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.5" ) ); //$NON-NLS-1$

    final ComboViewer comboRiverLine = new ComboViewer( riverLineGroup, SWT.DROP_DOWN | SWT.READ_ONLY );
    final GridData gridDatacomboRiverLine = new GridData( SWT.FILL, SWT.CENTER, true, false );
    gridDatacomboRiverLine.widthHint = comboWidth1;
    comboRiverLine.getControl().setLayoutData( gridDatacomboRiverLine );
    comboRiverLine.setContentProvider( new ArrayContentProvider() );
    comboRiverLine.setLabelProvider( new LabelProvider() );
    comboRiverLine.setInput( m_data.getRiverThemes() );

    final IViewerObservableValue targetRiverLine = ViewersObservables.observeSinglePostSelection( comboRiverLine );
    final IObservableValue modelRiverLine = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_RIVER_LINE );
    final IValidator riverLineNotNullValidator = new NotNullValidator<>( IKalypsoFeatureTheme.class, IStatus.WARNING, Messages.getString("ConfigureLengthSectionWizardPage.0") ); //$NON-NLS-1$
    m_binding.bindValue( targetRiverLine, modelRiverLine, riverLineNotNullValidator );

    /*
     * define properties page ( river name field selection combo river name selection combo, delta station spinner,
     * station field select combo)
     */
    final Group propertyGroup = new Group( composite, SWT.NONE );
    propertyGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    propertyGroup.setLayout( new GridLayout( 2, false ) );
    propertyGroup.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.6" ) ); //$NON-NLS-1$

    /* Water Body Name */
    final Label riverNameText = new Label( propertyGroup, SWT.NONE );
    riverNameText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    riverNameText.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.7" ) ); //$NON-NLS-1$

    final ComboViewer comboRiverNameField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDataComboRiverName = new GridData( SWT.FILL, SWT.END, true, false );
    gridDataComboRiverName.widthHint = comboWidth2;
    comboRiverNameField.getControl().setLayoutData( gridDataComboRiverName );

    comboRiverNameField.setContentProvider( new ArrayContentProvider() );
    comboRiverNameField.setLabelProvider( new PropertyNameLabelProvider() );

    final IObservableValue targetRiverNameInput = ViewersObservables.observeInput( comboRiverNameField );
    final IObservableValue modelRiverNameInput = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_RIVER_NAME_INPUT );
    m_binding.bindValue( targetRiverNameInput, modelRiverNameInput );

    final IViewerObservableValue targetRiverName = ViewersObservables.observeSinglePostSelection( comboRiverNameField );
    final IObservableValue modelRiverName = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_RIVER_NAME_PROPERTY );
    final IValidator riverNameNotNullValidator = new NotNullValidator<>( IValuePropertyType.class, IStatus.ERROR, Messages.getString("ConfigureLengthSectionWizardPage.1") ); //$NON-NLS-1$
    m_binding.bindValue( targetRiverName, modelRiverName, riverNameNotNullValidator );

    /* Selected water body */
    final Label selectedRiverText = new Label( propertyGroup, SWT.NONE );
    selectedRiverText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    selectedRiverText.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.8" ) ); //$NON-NLS-1$

    final ComboViewer comboSelectedRiverNameField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDataComboSelectedRiverField = new GridData( SWT.FILL, SWT.END, true, false );
    gridDataComboSelectedRiverField.widthHint = comboWidth2;
    comboSelectedRiverNameField.getControl().setLayoutData( gridDataComboSelectedRiverField );

    comboSelectedRiverNameField.setContentProvider( new ArrayContentProvider() );
    comboSelectedRiverNameField.setLabelProvider( new LabelProvider() );

    final IObservableValue targetSelectedRiverNameInput = ViewersObservables.observeInput( comboSelectedRiverNameField );
    final IObservableValue modelSelectedRiverNameInput = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_RIVER_NAMES );
    m_binding.bindValue( targetSelectedRiverNameInput, modelSelectedRiverNameInput );

    final IViewerObservableValue targetSelectedRiverName = ViewersObservables.observeSinglePostSelection( comboSelectedRiverNameField );
    final IObservableValue modelSelectedRiverName = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_SELECTED_RIVER_NAME );
    final IValidator selectedRiverNameNotNullValidator = new NotNullValidator<>( String.class, IStatus.ERROR, Messages.getString("ConfigureLengthSectionWizardPage.2") ); //$NON-NLS-1$
    m_binding.bindValue( targetSelectedRiverName, modelSelectedRiverName, selectedRiverNameNotNullValidator );


    /* Station from */
    final Label stationFromFieldText = new Label( propertyGroup, SWT.NONE );
    stationFromFieldText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stationFromFieldText.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.9" ) ); //$NON-NLS-1$

    final ComboViewer comboStationFromField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboStationFromField = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatacomboStationFromField.widthHint = comboWidth2;
    comboStationFromField.getControl().setLayoutData( gridDatacomboStationFromField );

    comboStationFromField.setContentProvider( new ArrayContentProvider() );
    comboStationFromField.setLabelProvider( new PropertyNameLabelProvider() );

    final IObservableValue targetStationFromInput = ViewersObservables.observeInput( comboStationFromField );
    final IObservableValue modelStationFromInput = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_STATION_FROM_INPUT );
    m_binding.bindValue( targetStationFromInput, modelStationFromInput );

    final IViewerObservableValue targetStationFrom = ViewersObservables.observeSinglePostSelection( comboStationFromField );
    final IObservableValue modelStationFrom = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_STATION_FROM_PROPERTY );
    m_binding.bindValue( targetStationFrom, modelStationFrom );

    /* Station to */
    final Label stationToFieldText = new Label( propertyGroup, SWT.NONE );
    stationToFieldText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stationToFieldText.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.10" ) ); //$NON-NLS-1$

    final ComboViewer comboStationToField = new ComboViewer( propertyGroup, SWT.NONE | SWT.READ_ONLY );
    final GridData gridDatacomboStationToField = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatacomboStationToField.widthHint = comboWidth2;
    comboStationToField.getControl().setLayoutData( gridDatacomboStationToField );

    comboStationToField.setContentProvider( new ArrayContentProvider() );
    comboStationToField.setLabelProvider( new PropertyNameLabelProvider() );

    final IObservableValue targetStationToInput = ViewersObservables.observeInput( comboStationToField );
    final IObservableValue modelStationToInput = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_STATION_TO_INPUT );
    m_binding.bindValue( targetStationToInput, modelStationToInput );

    final IViewerObservableValue targetStationTo = ViewersObservables.observeSinglePostSelection( comboStationToField );
    final IObservableValue modelStationTo = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_STATION_TO_PROPERTY );
    m_binding.bindValue( targetStationTo, modelStationTo );

    /* km or m checkbox */
    final Label dummy1 = new Label( propertyGroup, SWT.NONE );
    dummy1.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button buttonKmValues = new Button( propertyGroup, SWT.CHECK );
    buttonKmValues.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    buttonKmValues.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.11" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetUseKmValues = SWTObservables.observeSelection( buttonKmValues );
    final IObservableValue modelUseKmValues = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_USE_KM_VALUES );
    m_binding.bindValue( targetUseKmValues, modelUseKmValues );

    /* sampling distance */
    final Label samplingDistanceText = new Label( propertyGroup, SWT.NONE );
    samplingDistanceText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    samplingDistanceText.setText( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizardPage.12" ) ); //$NON-NLS-1$

    final Spinner samplingDistanceSpinner = new Spinner( propertyGroup, SWT.BORDER | SWT.TRAIL );
    final GridData gridDatastationSpinner = new GridData( SWT.FILL, SWT.END, true, false );
    gridDatastationSpinner.widthHint = 30;
    samplingDistanceSpinner.setLayoutData( gridDatastationSpinner );

    samplingDistanceSpinner.setValues( 0, 1, 1000, 0, 1, 100 );

    final ISWTObservableValue targetSamplingDistance = SWTObservables.observeSelection( samplingDistanceSpinner );
    final IObservableValue modelSamplingDistance = BeansObservables.observeValue( m_data, CreateLengthSectionData.PROPERTY_SAMPLING_DISTANCE );
    m_binding.bindValue( targetSamplingDistance, modelSamplingDistance );
  }
}
