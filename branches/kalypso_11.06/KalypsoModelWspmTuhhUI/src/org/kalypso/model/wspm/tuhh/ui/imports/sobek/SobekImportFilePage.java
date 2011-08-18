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
package org.kalypso.model.wspm.tuhh.ui.imports.sobek;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.ColumnLayout;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.DirectoryBinding;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.tuhh.ui.imports.sobek.SobekImportData.GUESS_STATION_STRATEGY;
import org.kalypso.model.wspm.tuhh.ui.utils.GuessStationPatternReplacer;
import org.kalypso.model.wspm.tuhh.ui.utils.GuessStationPatternValidator;
import org.kalypso.transformation.ui.CRSSelectionPanel;

/**
 * @author Gernot Belger
 */
public class SobekImportFilePage extends WizardPage
{
  private DatabindingWizardPage m_binding;

  private final SobekImportData m_data;

  protected SobekImportFilePage( final String pageName, final SobekImportData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Import SOBEK data" );
    setDescription( "Please select the SOBEK project directory on this page." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );

    final ColumnLayout columnLayout = new ColumnLayout();
    columnLayout.maxNumColumns = 1;
    panel.setLayout( columnLayout );

    m_binding = new DatabindingWizardPage( this, null );

    createInputDirControls( panel );
    createSrsControl( panel );
    createStationControls( panel );
  }

  private Control createInputDirControls( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( group );
    group.setText( "SOBEK Project" );

    final Label destinationLabel = new Label( group, SWT.NONE );
    destinationLabel.setText( "Input Directory" );

    // destination name entry field
    final FileAndHistoryData inputDir = m_data.getInputDir();

    final IObservableValue modelDir = BeansObservables.observeValue( inputDir, FileAndHistoryData.PROPERTY_FILE );
    final IObservableValue modelHistory = BeansObservables.observeValue( inputDir, FileAndHistoryData.PROPERTY_HISTORY );

    final DirectoryBinding directoryBinding = new DirectoryBinding( m_binding, modelDir, SWT.OPEN );

    final Control historyControl = directoryBinding.createDirectoryFieldWithHistory( group, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final String message = "Please select the SOBEK project directory:";
    final Button searchButton = directoryBinding.createDirectorySearchButton( group, historyControl, getWizard().getWindowTitle(), message );
    setButtonLayoutData( searchButton );

    return group;
  }

  private void createSrsControl( final Composite panel )
  {
    final CRSSelectionPanel crsPanel = new CRSSelectionPanel( panel, SWT.NONE );
    final IObservableValue target = crsPanel.observe();
    final IObservableValue model = BeansObservables.observeValue( m_data, SobekImportData.PROPERTY_SRS );

    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, "You must select a coordinate system" ) );
    m_binding.bindValue( binder );
  }

  private void createStationControls( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setText( "Station" );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( group );

    new Label( group, SWT.NONE ).setText( "Guess station..." );

    final ComboViewer combo = new ComboViewer( group );
    combo.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    combo.setContentProvider( new ArrayContentProvider() );
    combo.setLabelProvider( new LabelProvider() );
    combo.setInput( GUESS_STATION_STRATEGY.values() );

    final IViewerObservableValue targetStrategy = ViewersObservables.observeSinglePostSelection( combo );
    final IObservableValue modelStrategy = BeansObservables.observeValue( m_data, SobekImportData.PROPERTY_STATION_STRATEGY );
    m_binding.bindValue( targetStrategy, modelStrategy );

    new Label( group, SWT.NONE );

    /* Pattern control */
    final Label patternLabel = new Label( group, SWT.NONE );
    patternLabel.setText( "Pattern" );

    final Text patternField = new Text( group, SWT.BORDER );
    patternField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    patternField.setMessage( "Station Pattern" );
    patternField.setToolTipText( "Pattern for parsing the station from the sobek cross section. Use '<station>' and '*' as search tokens." );

    final GuessStationPatternReplacer replacer = new GuessStationPatternReplacer();
    replacer.createPatternButton( group, patternField );

    final ISWTObservableValue targetPattern = SWTObservables.observeText( patternField, SWT.Modify );
    final IObservableValue modelPattern = BeansObservables.observeValue( m_data, SobekImportData.PROPERTY_STATION_PATTERN );
    final DataBinder binderPattern = new DataBinder( targetPattern, modelPattern );
    binderPattern.addTargetAfterGetValidator( new GuessStationPatternValidator() );
    m_binding.bindValue( binderPattern );

    final ISWTObservableValue targetEnablement = SWTObservables.observeEditable( patternField );
    final IObservableValue modelEnablement = BeansObservables.observeValue( m_data, SobekImportData.PROPERTY_STATION_PATTERN_ENABLED );
    m_binding.bindValue( targetEnablement, modelEnablement );

    final ISWTObservableValue targetLabelEnablement = SWTObservables.observeEnabled( patternLabel );
    final IObservableValue modelLabelEnablement = BeansObservables.observeValue( m_data, SobekImportData.PROPERTY_STATION_PATTERN_ENABLED );
    m_binding.bindValue( targetLabelEnablement, modelLabelEnablement );
  }
}