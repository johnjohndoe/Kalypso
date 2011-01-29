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

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class SobekProfileFileChooser extends AbstractSobekFileChooser
{
  private static final String SETTINGS_PROFILE_EXPORT_FLOW_ZONE = "profileExportFlowZone"; //$NON-NLS-1$

  private static final String SETTINGS_EXPORT_BUILDINGS = "exportBuildings"; //$NON-NLS-1$

  private static final String STR_ZONE_LABEL = "Restrict To";

  private static final String STR_ZONE_TOOLTIP = "Restrict exported points to this flow zone";


  private String m_flowZone = IWspmTuhhConstants.MARKER_TYP_BORDVOLL;

  private boolean m_exportBuildings = true;

  public SobekProfileFileChooser( final SobekProfileExportFileChooserPage page, final IDialogSettings dialogSettings, final String filterLabel, final String extension )
  {
    super( page, dialogSettings, filterLabel, extension );

    readSettings();
  }

  @Override
  protected void createOtherControls( final Composite parent )
  {
    createZoneControls( parent );
    createTubeControls( parent );
  }

  private void createZoneControls( final Composite parent )
  {
    final Label zoneLabel = new Label( parent, SWT.NONE );
    zoneLabel.setText( STR_ZONE_LABEL );
    zoneLabel.setToolTipText( STR_ZONE_TOOLTIP );
    zoneLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );

    final String[] markerComponents = getMarkerComponents();
    readSettings( markerComponents );

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
          return "<Complete Profile>";

        final IComponent component = ComponentUtilities.getFeatureComponent( componentID );
        return ComponentUtilities.getComponentLabel( component );
      }
    } );
    zoneViewer.setInput( markerComponents );
    zoneViewer.setSelection( new StructuredSelection( m_flowZone ) );

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
    bridgeCheck.setText( "Export Buildings" );
    bridgeCheck.setSelection( m_exportBuildings );

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

  protected void handleBridgeCheckSelected( final boolean selection )
  {
    m_exportBuildings = selection;

    saveSettings();
  }

  private void readSettings( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return;

    /* Invert flag for better default behavior (we want true, but default emtpy settings is false) */
    m_exportBuildings = !settings.getBoolean( SETTINGS_EXPORT_BUILDINGS );
  }

  private void saveSettings( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return;

    /* Invert flag for better default behavior */
    settings.put( SETTINGS_EXPORT_BUILDINGS, !m_exportBuildings );
  }

  private void readSettings( final String[] markerComponents )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return;

    final String settingsZone = settings.get( SETTINGS_PROFILE_EXPORT_FLOW_ZONE );
    if( settingsZone == null )
      return;

    for( final String knownComponent : markerComponents )
    {
      final String id = knownComponent;
      if( settingsZone.equals( id ) )
      {
        m_flowZone = settingsZone;
        return;
      }
    }

    return;
  }

  protected void handleZoneChanged( final String flowZone )
  {
    m_flowZone = flowZone;

    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return;

    settings.put( SETTINGS_PROFILE_EXPORT_FLOW_ZONE, m_flowZone );
  }

  private String[] getMarkerComponents( )
  {
    final Collection<String> markers = new ArrayList<String>();

    markers.add( StringUtils.EMPTY );
    markers.add( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    markers.add( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    markers.add( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    return markers.toArray( new String[markers.size()] );
  }

  @Override
  public ISobekProfileExportOperation createOperation( final IProfileFeature[] profiles, final String idPattern, final String buildingSuffix )
  {
    final File file = getFile();
    if( file == null )
      return null;

    return new SobekDefExportOperation( file, profiles, idPattern, m_flowZone, m_exportBuildings, buildingSuffix );
  }
}
