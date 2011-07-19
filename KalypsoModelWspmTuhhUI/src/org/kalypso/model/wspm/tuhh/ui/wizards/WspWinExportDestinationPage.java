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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
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
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.conversion.FileToStringConverter;
import org.kalypso.commons.databinding.conversion.StringToFileConverter;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.DirectoryValueSelectionListener;
import org.kalypso.commons.databinding.validation.FileIsDirectoryValidator;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportData;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Monika Thül
 */
public class WspWinExportDestinationPage extends WizardPage
{
  private static final String SELECT_DESTINATION_MESSAGE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.2" ); //$NON-NLS-1$

  private static final String SELECT_DESTINATION_TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.3" ); //$NON-NLS-1$

  private static final String DESTINATION_BROWSE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.4" ); //$NON-NLS-1$

  private static final String DESTINATION_LABEL = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.5" ); //$NON-NLS-1$

  private static final String OVERWRITE_EXISTING_CHECK_LABEL = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.6" ); //$NON-NLS-1$

  private final WspWinExportData m_data;

  private DatabindingWizardPage m_binding;

  /**
   * Creates an instance of this class
   * 
   * @param aWorkbench
   *          IWorkbench
   */
  public WspWinExportDestinationPage( final String pageName, final WspWinExportData data )
  {
    super( pageName );

    m_data = data;

    setTitle( SELECT_DESTINATION_TITLE );
    setDescription( SELECT_DESTINATION_MESSAGE );
  }

  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );

    final Composite composite = new Composite( parent, SWT.NULL );
    composite.setLayout( new GridLayout() );
    composite.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_FILL | GridData.HORIZONTAL_ALIGN_FILL ) );
    composite.setFont( parent.getFont() );

    m_binding = new DatabindingWizardPage( this, null );

    createDestinationGroup( composite );
    createOptionsGroup( composite );

    setControl( composite );
  }

  /**
   * Create the export destination specification widgets
   * 
   * @param parent
   *          org.eclipse.swt.widgets.Composite
   */
  protected void createDestinationGroup( final Composite parent )
  {
    // destination specification group
    final Composite destinationSelectionGroup = new Composite( parent, SWT.NONE );
    destinationSelectionGroup.setLayout( new GridLayout( 3, false ) );
    destinationSelectionGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL ) );
    destinationSelectionGroup.setFont( parent.getFont() );

    final Label destinationLabel = new Label( destinationSelectionGroup, SWT.NONE );
    destinationLabel.setText( DESTINATION_LABEL );
    destinationLabel.setFont( parent.getFont() );

    // destination name entry field
    final ComboViewer viewer = new ComboViewer( destinationSelectionGroup, SWT.SINGLE | SWT.BORDER | SWT.DROP_DOWN );
    viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.getControl().setFont( parent.getFont() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );

    final IObservableValue targetInput = ViewersObservables.observeInput( viewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, WspWinExportData.PROPERTY_OUTPUT_DIR_HISTORY );
    m_binding.bindValue( targetInput, modelInput );

    // destination browse button
    final Button browseButton = new Button( destinationSelectionGroup, SWT.PUSH );
    browseButton.setText( DESTINATION_BROWSE );
    browseButton.setFont( parent.getFont() );
    setButtonLayoutData( browseButton );

    final ISWTObservableValue targetText = SWTObservables.observeText( viewer.getCombo() );
    final IObservableValue modelText = BeansObservables.observeValue( m_data, WspWinExportData.PROPERTY_OUTPUT_DIR );
    final DataBinder binder = new DataBinder( targetText, modelText );
    binder.setModelToTargetConverter( new FileToStringConverter() );
    binder.setTargetToModelConverter( new StringToFileConverter() );
    binder.addTargetAfterConvertValidator( new FileIsDirectoryValidator( IStatus.ERROR ) );
    m_binding.bindValue( binder );

    browseButton.addSelectionListener( new DirectoryValueSelectionListener( targetText, SELECT_DESTINATION_TITLE, SELECT_DESTINATION_MESSAGE ) );
  }

  private void createOptionsGroup( final Composite composite )
  {
    final Group group = new Group( composite, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    GridLayoutFactory.swtDefaults().applyTo( group );

    createOverwriteExisting( group );
  }

  /**
   * Create the button for checking if we should ask if we are going to overwrite existing files.
   */
  protected void createOverwriteExisting( final Group optionsGroup )
  {
    final Button overwriteExistingFilesCheckbox = new Button( optionsGroup, SWT.CHECK | SWT.LEFT );
    overwriteExistingFilesCheckbox.setText( OVERWRITE_EXISTING_CHECK_LABEL );
    overwriteExistingFilesCheckbox.setFont( optionsGroup.getFont() );

    final ISWTObservableValue target = SWTObservables.observeSelection( overwriteExistingFilesCheckbox );
    final IObservableValue model = BeansObservables.observeValue( m_data, WspWinExportData.PROPERTY_OVERWRITE_EXISTING );
    m_binding.bindValue( target, model );
  }
}