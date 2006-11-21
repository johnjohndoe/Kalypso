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
package org.kalypso.wizard;

import java.util.ArrayList;

import org.eclipse.jface.resource.ImageDescriptor;
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
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;

/**
 * 
 * @author kuepfer
 */
public class KalypsoNAProjectPreferences extends WizardPage
{

  private final GMLSchema m_modelSchema;

  private static final String SOIL_FT_NAME = "bodenkorrekturmember"; //$NON-NLS-1$

  private static final String CATCHMENT = "Catchment"; //$NON-NLS-1$

  String m_soilLayerNo = null;

  String m_kmChannelNo = null;

  private static final String KM_CHANNEL = "KMChannel"; //$NON-NLS-1$

  private String KMCHANNEL_MEMBER = "KMParameterMember"; //$NON-NLS-1$

  private Combo m_channelCombo;

  private Combo m_soilCombo;

  /**
   * @param schema
   *  
   */

  public KalypsoNAProjectPreferences( String pageName, GMLSchema schema )
  {
    super( pageName );
    setTitle( WizardMessages.getString("KalypsoNAProjectPreferences.Title") ); //$NON-NLS-1$
    setImageDescriptor( ImageProvider.IMAGE_KALYPSO_ICON_BIG );
    setDescription( WizardMessages.getString("KalypsoNAProjectPreferences.Description") ); //$NON-NLS-1$
    m_modelSchema = schema;
  }

  /**
   *  
   */

  public KalypsoNAProjectPreferences( String pageName, String title, ImageDescriptor titleImage, GMLSchema schema )
  {
    super( pageName, title, titleImage );
    m_modelSchema = schema;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite topComposite = new Composite( parent, SWT.NONE );
    topComposite.setLayout( new GridLayout() );
    Group soil = new Group( topComposite, SWT.NONE );
    soil.setText( WizardMessages.getString("KalypsoNAProjectPreferences.SoilGroupText") ); //$NON-NLS-1$
    soil.setLayout( new GridLayout( 2, true ) );
    soil.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    Label soilLabel = new Label( soil, SWT.NONE );
    soilLabel.setText( WizardMessages.getString("KalypsoNAProjectPreferences.SoilGroupLable") ); //$NON-NLS-1$
    m_soilCombo = new Combo( soil, SWT.READ_ONLY );
    FeatureType catchmentFT = m_modelSchema.getFeatureType( CATCHMENT );
    int maxOccursSoil = catchmentFT.getMaxOccurs( SOIL_FT_NAME );
    ArrayList noSoilLayer = new ArrayList();
    for( int i = 0; i < maxOccursSoil + 1; i++ )
      noSoilLayer.add( String.valueOf( i ) );
    m_soilCombo.setItems( (String[])noSoilLayer.toArray( new String[maxOccursSoil] ) );
    m_soilCombo.select( 1 );
    GridData soilComboGridData = new GridData();
    soilComboGridData.widthHint = 50;
    soilComboGridData.horizontalAlignment = GridData.END;
    m_soilCombo.setLayoutData( soilComboGridData );
    m_soilCombo.setToolTipText( WizardMessages.getString("KalypsoNAProjectPreferences.SoilDataToolTipText") ); //$NON-NLS-1$
    m_soilCombo.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        Combo combo = (Combo)e.widget;
        m_soilLayerNo = combo.getItem( combo.getSelectionIndex() );
        setPageComplete( validatePage() );
      }
    } );
    m_soilLayerNo = m_soilCombo.getItem( 1 );

    Group channel = new Group( topComposite, SWT.NONE );
    channel.setText( WizardMessages.getString("KalypsoNAProjectPreferences.KMChannelGroupText") ); //$NON-NLS-1$
    channel.setLayout( new GridLayout( 2, true ) );
    channel.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    Label channelLabel = new Label( channel, SWT.NONE );
    channelLabel.setText( WizardMessages.getString("KalypsoNAProjectPreferences.KMChannelGroupLable") ); //$NON-NLS-1$
    m_channelCombo = new Combo( channel, SWT.READ_ONLY );
    m_channelCombo.setLayout( new GridLayout() );
    FeatureType kmChannelFT = m_modelSchema.getFeatureType( KM_CHANNEL );
    int maxOccursKM = kmChannelFT.getMaxOccurs( KMCHANNEL_MEMBER );
    ArrayList noKMDischarge = new ArrayList();
    for( int i = 0; i < maxOccursKM + 1; i++ )
      noKMDischarge.add( String.valueOf( i ) );
    m_channelCombo.setItems( (String[])noKMDischarge.toArray( new String[maxOccursKM] ) );
    GridData channelComboGridData = new GridData();
    channelComboGridData.widthHint = 50;
    channelComboGridData.horizontalAlignment = GridData.END;
    m_channelCombo.setLayoutData( channelComboGridData );
    m_channelCombo
        .setToolTipText( WizardMessages.getString("KalypsoNAProjectPreferences.KMChannelDataLable") ); //$NON-NLS-1$
    m_channelCombo.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        Combo combo = (Combo)e.widget;
        m_kmChannelNo = combo.getItem( combo.getSelectionIndex() );
        setPageComplete( validatePage() );
      }
    } );
    m_channelCombo.select( maxOccursKM );
    m_kmChannelNo = m_channelCombo.getItem( maxOccursKM );
    setPageComplete( validatePage() );
    setControl( topComposite );
  }

  public String getSoilLayerNo()
  {
    return m_soilLayerNo;
  }

  public String getKMChannelNo()
  {
    return m_kmChannelNo;
  }

  boolean validatePage()
  {
    //  TODO wenn das NA Modell zukünftig ungleich fünf Abflüsse rechnen kann, dies entfernen!!
    if( !m_kmChannelNo.equals( String.valueOf( 5 ) ) )
    {
      setErrorMessage( WizardMessages.getString("KalypsoNAProjectPreferences.KMChannelErrorMessage") ); //$NON-NLS-1$
      return false;
    }
    setErrorMessage( null );
    return true;

  }
}
