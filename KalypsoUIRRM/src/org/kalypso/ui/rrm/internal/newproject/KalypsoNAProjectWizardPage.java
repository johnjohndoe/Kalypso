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

/*
 * Created on 31.01.2005
 *
 */
package org.kalypso.ui.rrm.internal.newproject;

import java.io.File;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusCompositeValue;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizardPage extends WizardPage
{
  private final KalypsoNAMappingData m_data;

  private Group m_fileGroup;

  private SourceMappingComposite m_sourceGroup;

  private Group m_mappingGroup;

  private IDataBinding m_binding;

  public KalypsoNAProjectWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final KalypsoNAMappingData data )
  {
    super( pageName, title, titleImage );

    m_data = data;

    setDescription( Messages.getString( "KalypsoNAProjectWizardPage.PageDescription" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NULL );
    topComposite.setLayout( new GridLayout() );
    topComposite.setFont( parent.getFont() );
    setControl( topComposite );

    m_binding = new DatabindingWizardPage( this, null );

    initializeDialogUnits( parent );

    final boolean isSkipEnabled = m_data.getSkip();

    // build wizard page
    final Button skipRadioButton = new Button( topComposite, SWT.CHECK );
    skipRadioButton.setText( getTitle() );
    skipRadioButton.setSelection( !isSkipEnabled );
    skipRadioButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleSkipCheckboxSelected( skipRadioButton.getSelection() );
      }
    } );

    final Control fileGroup = createFileGroup( topComposite );
    fileGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    createMappingGroup( topComposite );

    m_mappingGroup.setVisible( !isSkipEnabled );
    m_fileGroup.setVisible( !isSkipEnabled );
  }

  protected void handleSkipCheckboxSelected( final boolean selection )
  {
    m_data.setSkip( !selection );

    m_mappingGroup.setVisible( selection );
    m_fileGroup.setVisible( selection );

    validate();
  }

  private Control createFileGroup( final Composite parent )
  {
    m_fileGroup = new Group( parent, SWT.NULL );
    m_fileGroup.setLayout( new GridLayout( 3, false ) );
    m_fileGroup.setText( Messages.getString( "KalypsoNAProjectWizardPage.FileGroupText" ) ); //$NON-NLS-1$

    final FileChooserDelegateOpen openFileDelegate = new FileChooserDelegateOpen();
    openFileDelegate.addFilter( Messages.getString( "KalypsoNAProjectWizardPage_0" ), ".shp" ); //$NON-NLS-1$ //$NON-NLS-2$
    openFileDelegate.addFilter( Messages.getString( "KalypsoNAProjectWizardPage_2" ), "*.*" ); //$NON-NLS-1$ //$NON-NLS-2$

    final FileChooserGroup fileChooserGroup = new FileChooserGroup( openFileDelegate );
    fileChooserGroup.createControlsInGrid( m_fileGroup );

    fileChooserGroup.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        handleFileChanged( file );
      }
    } );

    final Label crsLabel = new Label( m_fileGroup, SWT.NONE );
    crsLabel.setText( Messages.getString( "KalypsoNAProjectWizardPage.CRSLabelText" ) ); //$NON-NLS-1$

    /* Coordinate system */
    final CRSSelectionPanel crsSelectionPanel = new CRSSelectionPanel( m_fileGroup, CRSSelectionPanel.NO_GROUP );
    crsSelectionPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    crsSelectionPanel.setSelectedCRS( m_data.getSrs() );
    crsSelectionPanel.setToolTipText( Messages.getString( "KalypsoNAProjectWizardPage.CRSTooltip" ) ); //$NON-NLS-1$

    crsSelectionPanel.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleCrsChanged( crsSelectionPanel.getSelectedCRS() );
      }
    } );

    return m_fileGroup;
  }

  protected void handleFileChanged( final File file )
  {
    m_data.setShapeFile( file );

    if( validate() )
      updateShape();
  }

  private void createMappingGroup( final Composite parent )
  {
    m_mappingGroup = new Group( parent, SWT.NONE );
    m_mappingGroup.setText( Messages.getString( "KalypsoNAProjectWizardPage.MappingGroupText" ) ); //$NON-NLS-1$
    m_mappingGroup.setLayout( new GridLayout() );

    m_mappingGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* A status composite for geometry type check */
    final StatusComposite shapeTypeStatus = new StatusComposite( m_mappingGroup, SWT.NONE );
    shapeTypeStatus.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final StatusCompositeValue targetStatus = new StatusCompositeValue( shapeTypeStatus );
    final IObservableValue modelStatus = BeansObservables.observeValue( m_data, KalypsoNAMappingData.PROPERTY_SHAPE_TYPE_STATUS );
    m_binding.bindValue( targetStatus, modelStatus );

    // Xxx

    final ScrolledForm scrolledForm = new ScrolledForm( m_mappingGroup, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    final GridData scrolledFormData = new GridData( SWT.FILL, SWT.FILL, true, true );
    scrolledFormData.heightHint = 250;
    scrolledForm.setLayoutData( scrolledFormData );

    scrolledForm.setExpandHorizontal( true );
    scrolledForm.setExpandVertical( true );

    final Composite body = scrolledForm.getBody();
    body.setLayout( new FillLayout() );

    m_sourceGroup = new SourceMappingComposite( body, SWT.NONE, m_data );

    // Reset buttons group
    final Button resetButton = new Button( m_mappingGroup, SWT.PUSH );
    resetButton.setText( Messages.getString( "KalypsoNAProjectWizardPage.ResetButtonText" ) ); //$NON-NLS-1$
    resetButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleResetSelection();
      }
    } );
  }

  protected void handleCrsChanged( final String selectedCRS )
  {
    m_data.setSrs( selectedCRS );

    if( validate() )
      updateShape();
  }

  protected void handleResetSelection( )
  {
    m_sourceGroup.resetSelection();
  }

  boolean validate( )
  {
    setMessage( null );
    final boolean pageComplete = doValidate();

    setPageComplete( pageComplete );
    return pageComplete;
  }

  private boolean doValidate( )
  {
    if( m_data.getSkip() )
      return true;

    if( m_data.getSrs() == null )
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotSupportedCS" ), IMessageProvider.ERROR ); //$NON-NLS-1$
      return false;
    }

    final File shapeFile = m_data.getShapeFile();
    if( shapeFile == null )
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageChooseFile" ), IMessageProvider.INFORMATION ); //$NON-NLS-1$
      return false;
    }

    if( !shapeFile.exists() )
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotValidFile" ), IMessageProvider.ERROR ); //$NON-NLS-1$
      return true;
    }

    if( !shapeFile.getName().toLowerCase().endsWith( ".shp" ) ) //$NON-NLS-1$
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageWrongSuffix" ), IMessageProvider.WARNING ); //$NON-NLS-1$
      return true;
    }

    return true;
  }

  protected void updateShape( )
  {
    m_data.readShapeFile( this );

    m_sourceGroup.updateSourceList();
  }
}