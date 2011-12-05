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
package org.kalypso.ui.rrm.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.rrm.i18n.Messages;

/**
 * @author kuepfer
 */
public class KalypsoNAProjectPreferences extends WizardPage
{
  String m_soilLayerNo = null;

  String m_kmChannelNo = null;

  private Combo m_channelCombo;

  private Combo m_soilCombo;

  public KalypsoNAProjectPreferences( final String pageName )
  {
    super( pageName );
    setTitle( Messages.getString( "KalypsoNAProjectPreferences.Title" ) ); //$NON-NLS-1$
    setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    setDescription( Messages.getString( "KalypsoNAProjectPreferences.Description" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
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
    m_soilCombo = new Combo( soil, SWT.READ_ONLY );
    final IFeatureType catchmentFT = GMLSchemaUtilities.getFeatureTypeQuiet( Catchment.FEATURE_CATCHMENT );

    final int maxOccursSoil = catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER ).getMaxOccurs();
    final List<String> noSoilLayer = new ArrayList<String>();
    for( int i = 0; i < maxOccursSoil + 1; i++ )
      noSoilLayer.add( String.valueOf( i ) );
    m_soilCombo.setItems( noSoilLayer.toArray( new String[maxOccursSoil] ) );
    m_soilCombo.select( 1 );
    final GridData soilComboGridData = new GridData();
    soilComboGridData.widthHint = 50;
    soilComboGridData.horizontalAlignment = GridData.END;
    m_soilCombo.setLayoutData( soilComboGridData );
    m_soilCombo.setToolTipText( Messages.getString( "KalypsoNAProjectPreferences.SoilDataToolTipText" ) ); //$NON-NLS-1$
    m_soilCombo.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Combo combo = (Combo) e.widget;
        m_soilLayerNo = combo.getItem( combo.getSelectionIndex() );
        setPageComplete( validatePage() );
      }
    } );
    m_soilLayerNo = m_soilCombo.getItem( 1 );

    final Group channel = new Group( topComposite, SWT.NONE );
    channel.setText( Messages.getString( "KalypsoNAProjectPreferences.KMChannelGroupText" ) ); //$NON-NLS-1$
    channel.setLayout( new GridLayout( 2, false ) );
    channel.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false ) );
    final Label channelLabel = new Label( channel, SWT.NONE );
    channelLabel.setText( Messages.getString( "KalypsoNAProjectPreferences.KMChannelGroupLable" ) ); //$NON-NLS-1$
    channelLabel.setLayoutData( new GridData(SWT.FILL, SWT.CENTER, true, true) );
    m_channelCombo = new Combo( channel, SWT.READ_ONLY );

    final IFeatureType kmChannelFT = GMLSchemaUtilities.getFeatureTypeQuiet( KMChannel.FEATURE_KM_CHANNEL );
    final int maxOccursKM = kmChannelFT.getProperty( KMChannel.MEMBER_PARAMETER ).getMaxOccurs();
    final ArrayList<String> noKMDischarge = new ArrayList<String>();
    for( int i = 0; i < maxOccursKM + 1; i++ )
      noKMDischarge.add( String.valueOf( i ) );
    m_channelCombo.setItems( noKMDischarge.toArray( new String[maxOccursKM] ) );
    final GridData channelComboGridData = new GridData();
    channelComboGridData.widthHint = 50;
    channelComboGridData.horizontalAlignment = GridData.END;
    m_channelCombo.setLayoutData( channelComboGridData );
    m_channelCombo.setToolTipText( Messages.getString( "KalypsoNAProjectPreferences.KMChannelDataLable" ) ); //$NON-NLS-1$
    m_channelCombo.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Combo combo = (Combo) e.widget;
        m_kmChannelNo = combo.getItem( combo.getSelectionIndex() );
        setPageComplete( validatePage() );
      }
    } );
    m_channelCombo.select( maxOccursKM );
    m_kmChannelNo = m_channelCombo.getItem( maxOccursKM );
    setPageComplete( validatePage() );
    setControl( topComposite );
  }

  public String getSoilLayerNo( )
  {
    return m_soilLayerNo;
  }

  public String getKMChannelNo( )
  {
    return m_kmChannelNo;
  }

  boolean validatePage( )
  {
    // TODO wenn das NA Modell zukünftig ungleich fünf Abflüsse rechnen kann, dies entfernen!!
    if( !m_kmChannelNo.equals( String.valueOf( 5 ) ) )
    {
      setErrorMessage( Messages.getString( "KalypsoNAProjectPreferences.KMChannelErrorMessage" ) ); //$NON-NLS-1$
      return false;
    }
    setErrorMessage( null );
    return true;

  }
}
