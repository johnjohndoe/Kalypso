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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.ui.export.ValidatingWizardPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class SobekProfileExportFileChooserPage extends ValidatingWizardPage
{
  private static final String STR_SUFFIX_TOOLTIP = Messages.getString( "SobekProfileExportFileChooserPage.1" ); //$NON-NLS-1$

  private static final String STR_ZONE_LABEL = Messages.getString( "SobekProfileFileChooser_0" ); //$NON-NLS-1$

  private static final String STR_ZONE_TOOLTIP = Messages.getString( "SobekProfileFileChooser_1" ); //$NON-NLS-1$

  private SobekExportInfo m_info;

  public SobekProfileExportFileChooserPage( )
  {
    super( "sobekProfileExportFileChooserPage" ); //$NON-NLS-1$

    setTitle( Messages.getString( "SobekProfileExportFileChooserPage_5" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "SobekProfileExportFileChooserPage_6" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    m_info = new SobekExportInfo( this, getDialogSettings() );
    m_info.readSettings();

    final Composite comp = new Composite( parent, SWT.NONE );
    comp.setLayout( new GridLayout() );

    final Composite idPanel = new Composite( comp, SWT.NONE );
    idPanel.setLayout( new GridLayout( 3, false ) );
    idPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    createExportDirControl( idPanel );
    createIDPatternControl( idPanel );
    createBuildingSuffixControl( idPanel );
    createNamePatternControl( idPanel );
    createZoneControls( idPanel );
    createTubeControls( parent );

    new SobekFrictionDatExportUI( m_info ).createControl( comp );

    setControl( comp );

    super.createControl( parent );
  }

  private void createZoneControls( final Composite parent )
  {
    final Label zoneLabel = new Label( parent, SWT.NONE );
    zoneLabel.setText( STR_ZONE_LABEL );
    zoneLabel.setToolTipText( STR_ZONE_TOOLTIP );
    zoneLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    final ComboViewer zoneViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    zoneViewer.getControl().setToolTipText( STR_ZONE_TOOLTIP );
    zoneViewer.setContentProvider( new ArrayContentProvider() );
    zoneViewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        final String componentID = (String) element;
        if( StringUtils.isBlank( componentID ) )
          return Messages.getString( "SobekProfileFileChooser_2" ); //$NON-NLS-1$

        final IComponent component = ComponentUtilities.getFeatureComponent( componentID );
        return ComponentUtilities.getComponentLabel( component );
      }
    } );

    final String[] markerComponents = m_info.getMarkerComponents();
    zoneViewer.setInput( markerComponents );
    final String flowZone = m_info.getFlowZone();
    zoneViewer.setSelection( new StructuredSelection( flowZone ) );

    new Label( parent, SWT.NONE );

    zoneViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleZoneChanged( (String) selection.getFirstElement() );
      }
    } );
  }

  private void createTubeControls( final Composite parent )
  {
    final Button bridgeCheck = new Button( parent, SWT.CHECK );
    bridgeCheck.setText( Messages.getString( "SobekProfileFileChooser_3" ) ); //$NON-NLS-1$
    bridgeCheck.setSelection( m_info.getExportBuildings() );

    bridgeCheck.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final boolean selection = bridgeCheck.getSelection();
        handleBridgeCheckSelected( selection );
      }
    } );
  }

  private void createExportDirControl( final Composite idPanel )
  {
    final FileChooserGroup dirChooser = m_info.getDirChooser();
    dirChooser.setDialogSettings( getDialogSettings() );

    dirChooser.setLabel( Messages.getString( "SobekProfileExportFileChooserPage.2" ) ); //$NON-NLS-1$

    dirChooser.createControlsInGrid( idPanel );
  }

  private void createNamePatternControl( final Composite parent )
  {
    final Label patternLabel = new Label( parent, SWT.NONE );
    patternLabel.setText( Messages.getString( "SobekProfileExportFileChooserPage.6" ) ); //$NON-NLS-1$
    patternLabel.setToolTipText( Messages.getString( "SobekProfileExportFileChooserPage.7" ) ); //$NON-NLS-1$

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setText( m_info.getNamePattern() );

    text.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleNamePatternChanged( text.getText() );
      }
    } );

    ProfilePatternInputReplacer.getINSTANCE().createPatternButton( parent, text );
  }

  private void createIDPatternControl( final Composite parent )
  {
    final Label patternLabel = new Label( parent, SWT.NONE );
    patternLabel.setText( Messages.getString( "SobekProfileExportFileChooserPage_9" ) ); //$NON-NLS-1$
    patternLabel.setToolTipText( Messages.getString( "SobekProfileExportFileChooserPage.3" ) ); //$NON-NLS-1$

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setText( m_info.getIdPattern() );

    text.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleIdPatternChanged( text.getText() );
      }
    } );

    ProfilePatternInputReplacer.getINSTANCE().createPatternButton( parent, text );
  }

  private void createBuildingSuffixControl( final Composite parent )
  {
    final Label suffixLabel = new Label( parent, SWT.NONE );
    suffixLabel.setText( Messages.getString( "SobekProfileExportFileChooserPage.4" ) ); //$NON-NLS-1$
    suffixLabel.setToolTipText( STR_SUFFIX_TOOLTIP );

    final Text suffixText = new Text( parent, SWT.SINGLE | SWT.BORDER );
    suffixText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    suffixText.setText( m_info.getIdSuffix() );
    suffixText.setToolTipText( STR_SUFFIX_TOOLTIP );

    suffixText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleSuffixModified( suffixText.getText() );
      }
    } );
  }

  protected void handleSuffixModified( final String idSuffix )
  {
    m_info.setIdSuffix( idSuffix );
  }

  protected void handleIdPatternChanged( final String currentValue )
  {
    m_info.setIdPattern( currentValue );
  }

  protected void handleNamePatternChanged( final String currentValue )
  {
    m_info.setNamePattern( currentValue );
  }

  protected void handleBridgeCheckSelected( final boolean selection )
  {
    m_info.setExportBridges( selection );
  }

  protected void handleZoneChanged( final String flowZone )
  {
    m_info.setFlowZone( flowZone );
  }

  public ISobekProfileExportOperation[] getOperations( final IProfileFeature[] profiles )
  {
    m_info.setProfiles( profiles );

    final Collection<ISobekProfileExportOperation> ops = new ArrayList<>();

    // FIXME: check if files exist and ask user to overwrite
    m_info.getTargetDir().mkdirs();

    ops.add( new SobekProfileDefExportOperation( m_info ) );
    ops.add( new SobekProfileDatExportOperation( m_info ) );
    ops.add( new SobekProfileShapeExportOperation( m_info ) );
    ops.add( new SobekFrictionDatExportOperation( m_info ) );
    ops.add( new SobekStructDefExportOperation( m_info ) );
    ops.add( new SobekStructDatExportOperation( m_info ) );
    ops.add( new SobekStructShapeExportOperation( m_info ) );

    return ops.toArray( new ISobekProfileExportOperation[ops.size()] );
  }

  @Override
  protected IMessageProvider validatePage( )
  {
    return m_info.validate();
  }

}
