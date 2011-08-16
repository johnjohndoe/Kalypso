/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.DirectoryBinding;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;

/**
 * @author Gernot Belger
 */
public class ImportAttachmentsOptionsPage extends WizardPage
{
  private final ImportAttachmentsData m_data;

  private DatabindingWizardPage m_binding;

  public ImportAttachmentsOptionsPage( final String pageName, final ImportAttachmentsData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Options" );
    setDescription( "Select the options for the attachments import on this page." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( panel );
    setControl( panel );

    // createStateControl( panel );
    createImportControls( panel );
    createExportZipControl( panel );

    setErrorMessage( null );
  }

// private void createStateControl( final Composite parent )
// {
// final Group group = new Group( parent, SWT.NONE );
// group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
// group.setText( "Selected State" );
// group.setLayout( new FillLayout() );
//
// final State state = m_data.getState();
// new StateViewer( group, m_binding, state, Mode.VIEW, null );
// }

  private void createImportControls( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setText( "Import Directory" );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( group );

    /* Import directory */
    createImportDirectoryControls( group );
    createImportPatternControls( group );
  }

  private void createImportDirectoryControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Source Directory" );

    final IObservableValue modelDir = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_IMPORT_DIR );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_IMPORT_DIR_HISTORY );

    final DirectoryBinding directoryBinding = new DirectoryBinding( m_binding, modelDir, SWT.OPEN );

    final Control historyControl = directoryBinding.createDirectoryFieldWithHistory( parent, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    historyControl.setToolTipText( "Attachments will be searched in this directory" );

    final String windowTitle = getWizard().getWindowTitle();
    final Button searchButton = directoryBinding.createDirectorySearchButton( parent, historyControl, windowTitle, "Select import directory:" );
    setButtonLayoutData( searchButton );
  }

  private void createImportPatternControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Source Pattern" );

    final Text patternField = new Text( parent, SWT.SINGLE | SWT.BORDER );
    patternField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    patternField.setMessage( "<File Pattern>" );
    patternField.setToolTipText( "Files will be attached to database elements according to this pattern." );

    final AttachmentPatternReplacer replacer = new AttachmentPatternReplacer();
    replacer.createPatternButton( parent, patternField );

    /* binding */
    final ISWTObservableValue targetField = SWTObservables.observeText( patternField, SWT.Modify );
    final IObservableValue modelField = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_IMPORT_PATTERN );

    final DataBinder binder = new DataBinder( targetField, modelField );
    binder.addTargetAfterConvertValidator( new StringBlankValidator( IStatus.ERROR, "'Pattern' field is empty" ) );
    binder.addTargetAfterConvertValidator( new AttachmentPatternValidator() );

    m_binding.bindValue( binder );
  }

  private void createExportZipControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setText( "Export ZIP" );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( group );

    final Label infoLabel = new Label( group, SWT.NONE );
    infoLabel.setText( "All found attachments will be zip'ed into this file for easy transfer to the web server." );
    infoLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 1 ) );

    new Label( group, SWT.NONE ).setText( "ZIP File" );

    final IObservableValue modelFile = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_ZIP_FILE );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, ImportAttachmentsData.PROPERTY_ZIP_HISTORY );

    final FileChooserDelegateSave delegate = new FileChooserDelegateSave();
    delegate.addFilter( "ZIP Files", "*.zip" ); //$NON-NLS-2$

    final FileBinding fileBinding = new FileBinding( m_binding, modelFile, delegate );
    final Control historyControl = fileBinding.createFileFieldWithHistory( group, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    fileBinding.createFileSearchButton( group, historyControl );
  }
}