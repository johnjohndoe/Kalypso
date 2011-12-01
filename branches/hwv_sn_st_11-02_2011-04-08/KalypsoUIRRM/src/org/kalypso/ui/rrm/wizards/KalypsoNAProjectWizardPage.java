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
package org.kalypso.ui.rrm.wizards;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectWizardPage extends WizardPage
{
  private Group m_fileGroup;

  private SourceMappingComposite m_sourceGroup;

  private Group m_mappingGroup;

  private String m_crs;

  private File m_shapeFile;

  private final IValuePropertyType[] m_targetProperties;

  private boolean m_skip = true;

  public KalypsoNAProjectWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final IValuePropertyType[] targetProperties )
  {
    super( pageName, title, titleImage );

    m_targetProperties = targetProperties;
    setDescription( Messages.getString( "KalypsoNAProjectWizardPage.PageDescription" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NULL );
    topComposite.setLayout( new GridLayout() );
    topComposite.setFont( parent.getFont() );
    setControl( topComposite );

    initializeDialogUnits( parent );

    // build wizard page
    final Button skipRadioButton = new Button( topComposite, SWT.CHECK );
    skipRadioButton.setText( getTitle() );
    skipRadioButton.setSelection( !m_skip );
    skipRadioButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleSkipCheckboxSelected( skipRadioButton.getSelection() );
      }
    } );

    final Control fileGroup = createFileGroup( topComposite );
    fileGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    createMappingGroup( topComposite );

    m_mappingGroup.setVisible( !m_skip );
    m_fileGroup.setVisible( !m_skip );
  }

  protected void handleSkipCheckboxSelected( final boolean selection )
  {
    m_skip = !selection;
    
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
    openFileDelegate.addFilter( Messages.getString("KalypsoNAProjectWizardPage_0"), ".shp" ); //$NON-NLS-1$ //$NON-NLS-2$
    openFileDelegate.addFilter( Messages.getString("KalypsoNAProjectWizardPage_2"), "*.*" ); //$NON-NLS-1$ //$NON-NLS-2$

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
    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final CRSSelectionPanel crsSelectionPanel = new CRSSelectionPanel( m_fileGroup, CRSSelectionPanel.NO_GROUP );
    crsSelectionPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    crsSelectionPanel.setSelectedCRS( m_crs );
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
    m_shapeFile = file;

    if( validate() )
      updateShape();
  }

  private void createMappingGroup( final Composite parent )
  {
    m_mappingGroup = new Group( parent, SWT.NONE );
    m_mappingGroup.setText( Messages.getString( "KalypsoNAProjectWizardPage.MappingGroupText" ) ); //$NON-NLS-1$
    final GridLayout topGroupLayout = new GridLayout();
    m_mappingGroup.setLayout( topGroupLayout );

    final GridData topGroupGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    m_mappingGroup.setLayoutData( topGroupGridData );

    final ScrolledForm scrolledForm = new ScrolledForm( m_mappingGroup, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    final GridData scrolledFormData = new GridData( SWT.FILL, SWT.FILL, true, true );
    scrolledFormData.heightHint = 250;
    scrolledForm.setLayoutData( scrolledFormData );

    scrolledForm.setExpandHorizontal( true );
    scrolledForm.setExpandVertical( true );

    final Composite body = scrolledForm.getBody();
    body.setLayout( new FillLayout() );

    m_sourceGroup = new SourceMappingComposite( body, SWT.NONE, m_targetProperties );

    // Reset buttons group
    final Button resetButton = new Button( m_mappingGroup, SWT.PUSH );
    resetButton.setText( Messages.getString( "KalypsoNAProjectWizardPage.ResetButtonText" ) ); //$NON-NLS-1$
    resetButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleResetSelection();
      }
    } );
  }

  protected void handleCrsChanged( final String selectedCRS )
  {
    m_crs = selectedCRS;

    if( validate() )
      updateShape();
  }

  protected void handleResetSelection( )
  {
    m_sourceGroup.resetSelection();
  }

  /**
   * This method returns a HashMap with the user defined mapping of the source to the target. key = (String) target
   * property value = (String) source property
   * 
   * @return map HashMap with the custom mapping
   */
  public Map<IValuePropertyType, IValuePropertyType> getMapping( )
  {
    if( m_skip )
      return null;
    
    if( m_sourceGroup == null )
      return null;

    return m_sourceGroup.getMapping();
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
    if( m_skip )
      return true;
    
    if( m_crs == null )
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotSupportedCS" ), IMessageProvider.ERROR ); //$NON-NLS-1$
      return false;
    }

    if( m_shapeFile == null )
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageChooseFile" ), IMessageProvider.INFORMATION ); //$NON-NLS-1$
      return false;
    }

    if( !m_shapeFile.exists() )
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageNotValidFile" ), IMessageProvider.ERROR ); //$NON-NLS-1$
      return true;
    }

    if( !m_shapeFile.getName().toLowerCase().endsWith( ".shp" ) ) //$NON-NLS-1$
    {
      setMessage( Messages.getString( "KalypsoNAProjectWizardPage.ErrorMessageWrongSuffix" ), IMessageProvider.WARNING ); //$NON-NLS-1$
      return true;
    }

    return true;
  }

  protected void updateShape( )
  {
    if( m_shapeFile == null )
      return;

    final String shapePath = m_shapeFile.getAbsolutePath();
    final String fileBase = FilenameUtils.removeExtension( shapePath );

    try
    {
      final GMLWorkspace shapeWorkspace = ShapeSerializer.deserialize( fileBase, m_crs );
      m_sourceGroup.setSourceWorkspace( shapeWorkspace );
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString("KalypsoNAProjectWizardPage_1"), e.getLocalizedMessage() ); //$NON-NLS-1$
      setMessage( msg, IMessageProvider.ERROR );
      setPageComplete( false );

      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, e.getLocalizedMessage(), e );
      ErrorDialog.openError( getShell(), Messages.getString( "KalypsoNAProjectWizardPage.TextMessageReadError" ), Messages.getString( "KalypsoNAProjectWizardPage.MessageReadError", shapePath ), status ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  public List< ? > getFeatureList( )
  {
    return m_sourceGroup.getSourceData();
  }
}