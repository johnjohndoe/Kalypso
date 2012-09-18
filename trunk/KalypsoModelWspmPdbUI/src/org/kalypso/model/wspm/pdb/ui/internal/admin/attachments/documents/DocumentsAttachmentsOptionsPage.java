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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.documents;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.DirectoryBinding;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class DocumentsAttachmentsOptionsPage extends WizardPage
{
  private final DocumentsAttachmentsData m_data;

  private DatabindingWizardPage m_binding;

  public DocumentsAttachmentsOptionsPage( final String pageName, final DocumentsAttachmentsData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString( "ImportAttachmentsOptionsPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportAttachmentsOptionsPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.fillDefaults().applyTo( panel );
    setControl( panel );

    createImportControls( panel );
    createExportZipControl( panel );

    setErrorMessage( null );
  }

  private void createImportControls( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setText( Messages.getString( "ImportAttachmentsOptionsPage.2" ) ); //$NON-NLS-1$
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( group );

    /* Import directory. */
    createImportDirectoryControls( group );
  }

  private void createImportDirectoryControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "ImportAttachmentsOptionsPage.3" ) ); //$NON-NLS-1$

    final IObservableValue modelDir = BeansObservables.observeValue( m_data, DocumentsAttachmentsData.PROPERTY_IMPORT_DIR );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, DocumentsAttachmentsData.PROPERTY_IMPORT_DIR_HISTORY );

    final DirectoryBinding directoryBinding = new DirectoryBinding( modelDir, SWT.OPEN );

    final Control historyControl = directoryBinding.createDirectoryFieldWithHistory( parent, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    historyControl.setToolTipText( Messages.getString( "ImportAttachmentsOptionsPage.4" ) ); //$NON-NLS-1$

    final String windowTitle = getWizard().getWindowTitle();
    final Button searchButton = directoryBinding.createDirectorySearchButton( parent, historyControl, windowTitle, Messages.getString( "ImportAttachmentsOptionsPage.5" ) ); //$NON-NLS-1$
    setButtonLayoutData( searchButton );

    directoryBinding.getHistoryBinder().addTargetAfterGetValidator( new StringBlankValidator( IStatus.ERROR, Messages.getString( "ImportAttachmentsOptionsPage.14" ) ) ); //$NON-NLS-1$

    directoryBinding.applyBinding( m_binding );
  }

  private void createExportZipControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setText( Messages.getString( "ImportAttachmentsOptionsPage.10" ) ); //$NON-NLS-1$
    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( group );

    final Label infoLabel = new Label( group, SWT.NONE );
    infoLabel.setText( Messages.getString( "ImportAttachmentsOptionsPage.11" ) ); //$NON-NLS-1$
    infoLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 3, 1 ) );

    new Label( group, SWT.NONE ).setText( Messages.getString( "ImportAttachmentsOptionsPage.12" ) ); //$NON-NLS-1$

    final IObservableValue modelFile = BeansObservables.observeValue( m_data, DocumentsAttachmentsData.PROPERTY_ZIP_FILE );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, DocumentsAttachmentsData.PROPERTY_ZIP_HISTORY );

    final FileChooserDelegateSave delegate = new FileChooserDelegateSave();
    delegate.addFilter( Messages.getString( "ImportAttachmentsOptionsPage.13" ), "*.zip" ); //$NON-NLS-1$//$NON-NLS-2$

    final FileBinding fileBinding = new FileBinding( m_binding, modelFile, delegate );
    final Control historyControl = fileBinding.createFileFieldWithHistory( group, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    fileBinding.createFileSearchButton( group, historyControl );
  }
}