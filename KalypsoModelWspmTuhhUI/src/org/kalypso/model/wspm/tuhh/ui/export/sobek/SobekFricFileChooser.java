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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.LeftBank;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.LeftForeland;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.MainChannel;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.RightBank;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.RightForeland;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class SobekFricFileChooser extends AbstractSobekFileChooser
{
  private static final String SETTING_ROUGHNESS = "roughnessId"; //$NON-NLS-1$

  private static final String SETTING_ZONES = "zones"; //$NON-NLS-1$

  private final static LabelProvider LABELPROVIDER = new LabelProvider()
  {
    /**
     * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText( final Object element )
    {
      final IComponent component = ProfilUtil.getFeatureComponent( (String) element );
      return ComponentUtilities.getComponentLabel( component );
    }
  };

  private static final IFlowZoneType[] ALL_ZONES = new IFlowZoneType[] { new LeftForeland(), new LeftBank(), new MainChannel(), new RightBank(), new RightForeland() };

  private String m_roughnessId = IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS;

  private IFlowZoneType[] m_zoneTypes = new IFlowZoneType[0];


  public SobekFricFileChooser( final SobekProfileExportFileChooserPage page, final IDialogSettings dialogSettings, final String filterLabel, final String extension )
  {
    super( page, dialogSettings, filterLabel, extension );

    readSettings( dialogSettings );
  }

  private void readSettings( final IDialogSettings dialogSettings )
  {
    if( dialogSettings == null )
      return;

    final String savedRoughness = dialogSettings.get( SETTING_ROUGHNESS );
    if( !StringUtils.isBlank( savedRoughness ) )
      m_roughnessId = savedRoughness;

    final String[] savedZones = dialogSettings.getArray( SETTING_ZONES );
    if( savedZones != null )
    {
      final Collection<IFlowZoneType> zoneTypes = new ArrayList<IFlowZoneType>( savedZones.length );
      for( final String className : savedZones )
      {
        final IFlowZoneType zone = findZone( className );
        if( zone != null )
          zoneTypes.add( zone );
      }
      m_zoneTypes = zoneTypes.toArray( new IFlowZoneType[zoneTypes.size()] );
    }
  }

  private IFlowZoneType findZone( final String className )
  {
    for( final IFlowZoneType zoneType : ALL_ZONES )
    {
      if( className.equals( zoneType.getClass().getName() ) )
        return zoneType;
    }

    return null;
  }

  @Override
  protected void createOtherControls( final Composite parent )
  {
    createRoughnessCombo( parent );
    createZoneChooser( parent );
  }

  private void createRoughnessCombo( final Composite parent )
  {
    final String[] input = new String[] { IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST };

    final Label roughnessLabel = new Label( parent, SWT.NONE );
    roughnessLabel.setText( "Roughness Type" );
    roughnessLabel.setToolTipText( "Export the chosen roughness type to sobek" );

    final ComboViewer roughnessViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    roughnessViewer.getControl().setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false, 2, 1 ) );
    roughnessViewer.setContentProvider( new ArrayContentProvider() );
    roughnessViewer.setLabelProvider( LABELPROVIDER );
    roughnessViewer.setInput( input );
    roughnessViewer.setSelection( new StructuredSelection( m_roughnessId ) );

    roughnessViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleRoughnessSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );
  }

  protected void handleRoughnessSelectionChanged( final IStructuredSelection selection )
  {
    m_roughnessId = (String) selection.getFirstElement();

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      dialogSettings.put( SETTING_ROUGHNESS, m_roughnessId );
  }

  private void createZoneChooser( final Composite parent )
  {
    final Label zoneLabel = new Label( parent, SWT.NONE );
    zoneLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false ) );
    zoneLabel.setText( "Flow Zones" );
    zoneLabel.setToolTipText( "Build sobek friction as average from these flow zones" );

    final Table table = new Table( parent, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    final CheckboxTableViewer zoneViewer = new CheckboxTableViewer( table );

    zoneViewer.setLabelProvider( new LabelProvider() );
    zoneViewer.setContentProvider( new ArrayContentProvider() );
    zoneViewer.setInput( ALL_ZONES );

    zoneViewer.setCheckedElements( m_zoneTypes );

    zoneViewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        handleZonesChecked( zoneViewer.getCheckedElements() );
      }
    } );
  }

  protected void handleZonesChecked( final Object[] objects )
  {
    m_zoneTypes = new IFlowZoneType[objects.length];
    for( int i = 0; i < objects.length; i++ )
      m_zoneTypes[i] = (IFlowZoneType) objects[i];

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      final String[] zoneNames = new String[m_zoneTypes.length];
      for( int i = 0; i < zoneNames.length; i++ )
        zoneNames[i] = m_zoneTypes[i].getClass().getName();
      dialogSettings.put( SETTING_ZONES, zoneNames );
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.sobek.AbstractSobekFileChooser#createOperation(org.kalypso.model.wspm.core.gml.IProfileFeature[],
   *      java.lang.String, java.lang.String)
   */
  @Override
  public ISobekProfileExportOperation createOperation( final IProfileFeature[] profiles, final String idPattern, final String buildingSuffix )
  {
    final File file = getFile();
    if( file == null )
      return null;

    return new SobekFricExportOperation( file, profiles, m_zoneTypes, m_roughnessId, idPattern );
  }
}
