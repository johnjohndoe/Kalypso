/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 *
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 *
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 *
 * and
 *
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * Contact:
 *
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 *
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.rrm.internal.newproject;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectPreferences extends WizardPage
{
  private Integer m_soilLayerNo = 1;

  private Integer m_kmChannelNo = 5;

  public KalypsoNAProjectPreferences( final String pageName )
  {
    super( pageName );
    setTitle( Messages.getString( "KalypsoNAProjectPreferences.Title" ) ); //$NON-NLS-1$
    setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    setDescription( Messages.getString( "KalypsoNAProjectPreferences.Description" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NONE );
    topComposite.setLayout( new GridLayout() );
    final Group soil = new Group( topComposite, SWT.NONE );
    soil.setText( Messages.getString( "KalypsoNAProjectPreferences.SoilGroupText" ) ); //$NON-NLS-1$
    soil.setLayout( new GridLayout( 2, false ) );
    soil.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false ) );
    final Label soilLabel = new Label( soil, SWT.NONE );
    soilLabel.setLayoutData( new GridData(SWT.FILL, SWT.CENTER, true, true) );
    soilLabel.setText( Messages.getString( "KalypsoNAProjectPreferences.SoilGroupLable" ) ); //$NON-NLS-1$

    /* Selection of number of soil layers */
    final ComboViewer soilCombo = new ComboViewer( soil, SWT.READ_ONLY | SWT.DROP_DOWN );
    soilCombo.setContentProvider( new ArrayContentProvider() );
    soilCombo.setLabelProvider( new LabelProvider() );

    final Integer[] soilInput = getSoilInput();
    soilCombo.setInput( soilInput );

    soilCombo.setSelection( new StructuredSelection( m_soilLayerNo ) );

    final GridData soilComboGridData = new GridData();
    soilComboGridData.widthHint = 50;
    soilComboGridData.horizontalAlignment = GridData.END;
    soilCombo.getControl().setLayoutData( soilComboGridData );

    soilCombo.getControl().setToolTipText( Messages.getString( "KalypsoNAProjectPreferences.SoilDataToolTipText" ) ); //$NON-NLS-1$
    soilCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleSoilSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    /* Selection of number of KM-Parameters per channel */
    final Group channel = new Group( topComposite, SWT.NONE );
    channel.setText( Messages.getString( "KalypsoNAProjectPreferences.KMChannelGroupText" ) ); //$NON-NLS-1$
    channel.setLayout( new GridLayout( 2, false ) );
    channel.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false ) );
    final Label channelLabel = new Label( channel, SWT.NONE );
    channelLabel.setText( Messages.getString( "KalypsoNAProjectPreferences.KMChannelGroupLable" ) ); //$NON-NLS-1$
    channelLabel.setLayoutData( new GridData(SWT.FILL, SWT.CENTER, true, true) );

    final ComboViewer channelCombo = new ComboViewer( channel, SWT.READ_ONLY | SWT.DROP_DOWN );
    channelCombo.setContentProvider( new ArrayContentProvider() );
    channelCombo.setLabelProvider( new LabelProvider() );

    final Integer[] channelInput = getChannelInput();
    channelCombo.setInput( channelInput );

    final GridData channelComboGridData = new GridData();
    channelComboGridData.widthHint = 50;
    channelComboGridData.horizontalAlignment = GridData.END;
    channelCombo.getControl().setLayoutData( channelComboGridData );

    channelCombo.getControl().setToolTipText( Messages.getString( "KalypsoNAProjectPreferences.KMChannelDataLable" ) ); //$NON-NLS-1$

    channelCombo.getControl().setEnabled( false );

    channelCombo.setSelection( new StructuredSelection( m_kmChannelNo ) );

    channelCombo.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        handleChannelSelectionChanged( (IStructuredSelection) event.getSelection() );
      }
    } );

    setPageComplete( validatePage() );
    setControl( topComposite );
  }

  protected void handleChannelSelectionChanged( final IStructuredSelection selection )
  {
    m_kmChannelNo = (Integer) selection.getFirstElement();

    setPageComplete( validatePage() );
  }

  private Integer[] getChannelInput( )
  {
    final IFeatureType kmChannelFT = GMLSchemaUtilities.getFeatureTypeQuiet( KMChannel.FEATURE_KM_CHANNEL );
    final int maxOccursKM = kmChannelFT.getProperty( KMChannel.MEMBER_PARAMETER ).getMaxOccurs();

    final Collection<Integer> noKMDischarge = new ArrayList<>();
    for( int i = 0; i < maxOccursKM; i++ )
      noKMDischarge.add( i + 1 );

    return noKMDischarge.toArray( new Integer[noKMDischarge.size()] );
  }

  protected void handleSoilSelectionChanged( final IStructuredSelection selection )
  {
    m_soilLayerNo = (Integer) selection.getFirstElement();

    setPageComplete( validatePage() );
  }

  private Integer[] getSoilInput( )
  {
    final IFeatureType catchmentFT = GMLSchemaUtilities.getFeatureTypeQuiet( Catchment.FEATURE_CATCHMENT );

    final int maxOccursSoil = catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER ).getMaxOccurs();

    final List<Integer> noSoilLayer = new ArrayList<>();
    for( int i = 0; i < maxOccursSoil; i++ )
      noSoilLayer.add( i + 1 );

    return noSoilLayer.toArray( new Integer[noSoilLayer.size()] );
  }

  public int getSoilLayerNo( )
  {
    return m_soilLayerNo;
  }

  public int getKMChannelNo( )
  {
    return m_kmChannelNo;
  }

  private boolean validatePage( )
  {
    // TODO wenn das NA Modell zukünftig ungleich fünf Abflüsse rechnen kann, dies entfernen!!
    if( !m_kmChannelNo.equals( 5 ) )
    {
      setErrorMessage( Messages.getString( "KalypsoNAProjectPreferences.KMChannelErrorMessage" ) ); //$NON-NLS-1$
      return false;
    }
    setErrorMessage( null );
    return true;
  }
}
