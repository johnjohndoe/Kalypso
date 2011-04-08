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
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class SobekFrictionDatExportUI
{
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

  private final SobekExportInfo m_info;

  public SobekFrictionDatExportUI( final SobekExportInfo info )
  {
    m_info = info;
  }

  public final void createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 3, false ) );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( "Rauheit" );

    createRoughnessCombo( group );
    createZoneChooser( group );

    if( group.getChildren().length == 0 )
      group.dispose();
  }

  private void createRoughnessCombo( final Composite parent )
  {
    final String[] input = new String[] { IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST };

    final Label roughnessLabel = new Label( parent, SWT.NONE );
    roughnessLabel.setText( Messages.getString("SobekFricFileChooser_0") ); //$NON-NLS-1$
    roughnessLabel.setToolTipText( Messages.getString("SobekFricFileChooser_1") ); //$NON-NLS-1$

    final ComboViewer roughnessViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    roughnessViewer.getControl().setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false, 2, 1 ) );
    roughnessViewer.setContentProvider( new ArrayContentProvider() );
    roughnessViewer.setLabelProvider( LABELPROVIDER );
    roughnessViewer.setInput( input );
    roughnessViewer.setSelection( new StructuredSelection( m_info.getRoughnessID() ) );

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
    final String roughnessId = (String) selection.getFirstElement();
    m_info.setRoughnessID( roughnessId );
  }

  private void createZoneChooser( final Composite parent )
  {
    final Label zoneLabel = new Label( parent, SWT.NONE );
    zoneLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false ) );
    zoneLabel.setText( Messages.getString("SobekFricFileChooser_2") ); //$NON-NLS-1$
    zoneLabel.setToolTipText( Messages.getString("SobekFricFileChooser_3") ); //$NON-NLS-1$

    final Table table = new Table( parent, SWT.CHECK | SWT.BORDER | SWT.V_SCROLL );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true, 2, 1 ) );
    final CheckboxTableViewer zoneViewer = new CheckboxTableViewer( table );

    zoneViewer.setLabelProvider( new LabelProvider() );
    zoneViewer.setContentProvider( new ArrayContentProvider() );
    zoneViewer.setInput( m_info.getAllRoughnessZones() );
    zoneViewer.setCheckedElements( m_info.getRoughnessZoneTypes() );

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
    final IFlowZoneType[] zoneTypes = new IFlowZoneType[objects.length];
    for( int i = 0; i < objects.length; i++ )
      zoneTypes[i] = (IFlowZoneType) objects[i];

    m_info.setRoughnessZoneTypes( zoneTypes );
  }
}
