/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.DirectoryBinding;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.java.io.FileExtensions;
import org.kalypso.contribs.java.io.FilePattern;

/**
 * @author Holger Albert
 */
public class EwawiImportFilesPage extends WizardPage
{
  private final EwawiImportData m_data;

  public EwawiImportFilesPage( final EwawiImportData data )
  {
    super( "ewawiImportFilesPage" ); //$NON-NLS-1$

    m_data = data;

    setTitle( "Quellenauswahl" );
    setDescription( "Auswahl von .pro und .sta Dateien sowie dem Foto- und dem Dokumentenvereichnis." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* Create the databinding. */
    final DatabindingWizardPage dataBinding = new DatabindingWizardPage( this, null );

    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    final GridLayout mainLayout = new GridLayout( 2, false );
    mainLayout.marginHeight = 0;
    mainLayout.marginWidth = 0;
    main.setLayout( mainLayout );

    /* Create a label. */
    final Label proFileLabel = new Label( main, SWT.NONE );
    proFileLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    proFileLabel.setText( ".pro Datei:" );

    /* Create the .pro file controls. */
    createProFileControls( main, dataBinding );

    /* Create a label. */
    final Label staFileLabel = new Label( main, SWT.NONE );
    staFileLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    staFileLabel.setText( ".sta Datei:" );

    /* Create the .sta file controls. */
    createStaFileControls( main, dataBinding );

    /* Create a label. */
    final Label fotoDirectoryLabel = new Label( main, SWT.NONE );
    fotoDirectoryLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    fotoDirectoryLabel.setText( "Bilderverzeichnis [optional]:" );

    /* Create the foto directory controls. */
    createFotoDirectoryControls( main, dataBinding );

    /* Create a label. */
    final Label documentDirectoryLabel = new Label( main, SWT.NONE );
    documentDirectoryLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    documentDirectoryLabel.setText( "Dokumentenverzeichnis [optional]:" );

    /* Create the document directory controls. */
    createDocumentDirectoryControls( main, dataBinding );

    /* Create a label. */
    final Label shpFileLabel = new Label( main, SWT.NONE );
    shpFileLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    shpFileLabel.setText( "Gewässershape [optional]:" );

    /* Create the .shp file controls. */
    createShpFileControls( main, dataBinding );

    /* Set the control to the page. */
    setControl( main );
  }

  private void createProFileControls( final Composite main, final DatabindingWizardPage dataBinding )
  {
    final FileAndHistoryData sourceFile = m_data.getProFile();
    final FilePattern filePattern = new FilePattern( "*.pro", "EWAWI+ .pro Datei" ); //$NON-NLS-1$

    final IObservableValue modelFile = BeansObservables.observeValue( sourceFile, FileAndHistoryData.PROPERTY_FILE );
    final FileChooserDelegateOpen delegate = createFileChooserDelegate( filePattern );
    final FileBinding fileBinding = new FileBinding( dataBinding, modelFile, delegate );

    final IObservableValue modelHistory = BeansObservables.observeValue( sourceFile, FileAndHistoryData.PROPERTY_HISTORY );
    final Control historyControl = fileBinding.createFileFieldWithHistory( main, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button fileButton = fileBinding.createFileSearchButton( main, historyControl );
    fileButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
  }

  private void createStaFileControls( final Composite main, final DatabindingWizardPage dataBinding )
  {
    final FileAndHistoryData sourceFile = m_data.getStaFile();
    final FilePattern filePattern = new FilePattern( "*.sta", "EWAWI+ .sta Datei" ); //$NON-NLS-1$

    final IObservableValue modelFile = BeansObservables.observeValue( sourceFile, FileAndHistoryData.PROPERTY_FILE );
    final FileChooserDelegateOpen delegate = createFileChooserDelegate( filePattern );
    final FileBinding fileBinding = new FileBinding( dataBinding, modelFile, delegate );

    final IObservableValue modelHistory = BeansObservables.observeValue( sourceFile, FileAndHistoryData.PROPERTY_HISTORY );
    final Control historyControl = fileBinding.createFileFieldWithHistory( main, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button fileButton = fileBinding.createFileSearchButton( main, historyControl );
    fileButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
  }

  private void createFotoDirectoryControls( final Composite main, final DatabindingWizardPage dataBinding )
  {
    final IObservableValue modelDir = BeansObservables.observeValue( m_data, EwawiImportData.PROPERTY_FOTO_DIRECTORY );
    final DirectoryBinding directoryBinding = new DirectoryBinding( modelDir, SWT.SAVE );

    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, EwawiImportData.PROPERTY_FOTO_DIRECTORY_HISTORY );
    final Control historyControl = directoryBinding.createDirectoryFieldWithHistory( main, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button directoryButton = directoryBinding.createDirectorySearchButton( main, historyControl, "Auswahl Verzeichnis", "Verzeichnis auswählen" );
    directoryButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    directoryBinding.applyBinding( dataBinding );
  }

  private void createDocumentDirectoryControls( final Composite main, final DatabindingWizardPage dataBinding )
  {
    final IObservableValue modelDir = BeansObservables.observeValue( m_data, EwawiImportData.PROPERTY_DOCUMENT_DIRECTORY );
    final DirectoryBinding directoryBinding = new DirectoryBinding( modelDir, SWT.SAVE );

    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, EwawiImportData.PROPERTY_DOCUMENT_DIRECTORY_HISTORY );
    final Control historyControl = directoryBinding.createDirectoryFieldWithHistory( main, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button directoryButton = directoryBinding.createDirectorySearchButton( main, historyControl, "Auswahl Verzeichnis", "Verzeichnis auswählen" );
    directoryButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    directoryBinding.applyBinding( dataBinding );
  }

  private void createShpFileControls( final Composite main, final DatabindingWizardPage dataBinding )
  {
    final FileAndHistoryData sourceFile = m_data.getRiverShapeData().getShpFile();
    final FilePattern filePattern = new FilePattern( "*.shp", "ESRI .shp Datei" ); //$NON-NLS-1$

    final IObservableValue modelFile = BeansObservables.observeValue( sourceFile, FileAndHistoryData.PROPERTY_FILE );
    final FileChooserDelegateOpen delegate = createFileChooserDelegate( filePattern );
    final FileBinding fileBinding = new FileBinding( dataBinding, modelFile, delegate );

    final IObservableValue modelHistory = BeansObservables.observeValue( sourceFile, FileAndHistoryData.PROPERTY_HISTORY );
    final Control historyControl = fileBinding.createFileFieldWithHistory( main, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final Button fileButton = fileBinding.createFileSearchButton( main, historyControl );
    fileButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );
  }

  private FileChooserDelegateOpen createFileChooserDelegate( final FilePattern pattern )
  {
    final FileChooserDelegateOpen delegate = new FileChooserDelegateOpen();
    delegate.addFilter( pattern );
    delegate.addFilter( FileExtensions.ALL_FILES );

    return delegate;
  }
}