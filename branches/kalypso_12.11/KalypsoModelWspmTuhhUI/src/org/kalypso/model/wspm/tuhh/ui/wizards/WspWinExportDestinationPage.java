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

import org.apache.commons.lang3.StringUtils;
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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.DirectoryBinding;
import org.kalypso.commons.databinding.validation.FileIsAsciiPrintable;
import org.kalypso.commons.databinding.validation.StringMustNotContainValidator;
import org.kalypso.commons.databinding.validation.StringTooLongValidator;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportData;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportProjectData;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.wspwin.core.WspCfg.TYPE;

/**
 * @author Monika Thül
 */
public class WspWinExportDestinationPage extends WizardPage
{
  private static final int WSPWIN_MAX_PATH_LENGTH = 100;

  private static final String SELECT_DESTINATION_MESSAGE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.2" ); //$NON-NLS-1$

  private static final String SELECT_DESTINATION_TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportPage.3" ); //$NON-NLS-1$

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
    final Group destinationSelectionGroup = new Group( parent, SWT.NONE );
    destinationSelectionGroup.setLayout( new GridLayout( 3, false ) );
    destinationSelectionGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL ) );
    destinationSelectionGroup.setFont( parent.getFont() );
    destinationSelectionGroup.setText( Messages.getString( "WspWinExportDestinationPage.0" ) ); //$NON-NLS-1$

    final Label destinationLabel = new Label( destinationSelectionGroup, SWT.NONE );
    destinationLabel.setText( DESTINATION_LABEL );
    destinationLabel.setFont( parent.getFont() );

    // destination name entry field
    final IObservableValue modelDir = BeansObservables.observeValue( m_data, WspWinExportProjectData.PROPERTY_OUTPUT_DIR );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, WspWinExportProjectData.PROPERTY_OUTPUT_DIR_HISTORY );

    final DirectoryBinding directoryBinding = new DirectoryBinding( modelDir, SWT.SAVE );

    final Control historyControl = directoryBinding.createDirectoryFieldWithHistory( destinationSelectionGroup, modelHistory );
    historyControl.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final String tooLongMessage = String.format( Messages.getString("WspWinExportDestinationPage.3"), WSPWIN_MAX_PATH_LENGTH ); //$NON-NLS-1$
    final DataBinder historyBinder = directoryBinding.getHistoryBinder();
    historyBinder.addTargetAfterGetValidator( new StringTooLongValidator( IStatus.WARNING, tooLongMessage, WSPWIN_MAX_PATH_LENGTH ) );
    historyBinder.addTargetAfterGetValidator( new StringMustNotContainValidator( IStatus.WARNING, Messages.getString("WspWinExportDestinationPage.4"), " " ) );  //$NON-NLS-1$//$NON-NLS-2$
    historyBinder.addTargetAfterConvertValidator( new FileIsAsciiPrintable( IStatus.WARNING, Messages.getString("WspWinExportDestinationPage.5") ) ); //$NON-NLS-1$

    final Button searchButton = directoryBinding.createDirectorySearchButton( destinationSelectionGroup, historyControl, SELECT_DESTINATION_TITLE, SELECT_DESTINATION_MESSAGE );
    setButtonLayoutData( searchButton );

    directoryBinding.applyBinding( m_binding );
  }

  private void createOptionsGroup( final Composite composite )
  {
    final Group group = new Group( composite, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( group );
    group.setText( Messages.getString( "WspWinExportDestinationPage.1" ) ); //$NON-NLS-1$

    createOverwriteExisting( group );
    createProjectType( group );
    createRoughnessType( group );
    createPreferClasses( group, Messages.getString("WspWinExportDestinationPage.6"), WspWinExportData.PROPERTY_PREFER_ROUGHNESS_CLASSES ); //$NON-NLS-1$
    createPreferClasses( group, Messages.getString("WspWinExportDestinationPage.7"), WspWinExportData.PROPERTY_PREFER_VEGETATION_CLASSES ); //$NON-NLS-1$
  }

  private void createRoughnessType( final Group group )
  {
    final Label label = new Label( group, SWT.NONE );
    label.setText( Messages.getString("WspWinExportDestinationPage.8") ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( group );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        final String componentID = (String) element;
        if( StringUtils.isBlank( componentID ) )
          return Messages.getString("WspWinExportDestinationPage.9"); //$NON-NLS-1$

        final IComponent component = ComponentUtilities.getFeatureComponent( componentID );
        return ComponentUtilities.getComponentLabel( component );
      }
    } );

    final String[] input = new String[] { StringUtils.EMPTY, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST };
    viewer.setInput( input );

    /* binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, WspWinExportData.PROPERTY_ROUGHNESS_TYPE );
    m_binding.bindValue( targetSelection, modelSelection );
  }

  private void createPreferClasses( final Group group, final String text, final String property )
  {
    final Label label = new Label( group, SWT.NONE );
    label.setText( text );

    final ComboViewer viewer = new ComboViewer( group );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        final Boolean value = (Boolean) element;
        if( value == false )
          return Messages.getString("WspWinExportDestinationPage.10"); //$NON-NLS-1$
        else
          return Messages.getString("WspWinExportDestinationPage.11"); //$NON-NLS-1$
      }
    } );

    viewer.setInput( new Boolean[] { Boolean.FALSE, Boolean.TRUE } );

    /* binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, property );
    m_binding.bindValue( targetSelection, modelSelection );
  }

  /**
   * Create the button for checking if we should ask if we are going to overwrite existing files.
   */
  protected void createOverwriteExisting( final Group optionsGroup )
  {
    final Button overwriteExistingFilesCheckbox = new Button( optionsGroup, SWT.CHECK | SWT.LEFT );
    overwriteExistingFilesCheckbox.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    overwriteExistingFilesCheckbox.setText( OVERWRITE_EXISTING_CHECK_LABEL );
    overwriteExistingFilesCheckbox.setFont( optionsGroup.getFont() );

    final ISWTObservableValue target = SWTObservables.observeSelection( overwriteExistingFilesCheckbox );
    final IObservableValue model = BeansObservables.observeValue( m_data, WspWinExportProjectData.PROPERTY_OVERWRITE_EXISTING );
    m_binding.bindValue( target, model );
  }

  private void createProjectType( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString( "WspWinExportDestinationPage.2" ) ); //$NON-NLS-1$

    final ComboViewer viewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    viewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setInput( TYPE.values() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, WspWinExportData.PROPERTY_PROJECT_TYPE );
    m_binding.bindValue( target, model );
  }
}