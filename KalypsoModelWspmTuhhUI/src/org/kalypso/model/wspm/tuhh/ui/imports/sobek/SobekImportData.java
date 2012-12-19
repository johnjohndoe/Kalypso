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
package org.kalypso.model.wspm.tuhh.ui.imports.sobek;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.utils.GuessStationPattern;
import org.kalypso.model.wspm.ui.profil.wizard.importProfile.ImportProfileWizard;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Gernot Belger
 */
public class SobekImportData extends AbstractModelObject
{
  public enum GUESS_STATION_STRATEGY
  {
    doNotGuess(Messages.getString("SobekImportData_0")), //$NON-NLS-1$
    sectionId(Messages.getString("SobekImportData_1")), //$NON-NLS-1$
    sectionNm(Messages.getString("SobekImportData_2")); //$NON-NLS-1$

    private final String m_label;

    GUESS_STATION_STRATEGY( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  public static final String PROPERTY_SRS = "srs"; //$NON-NLS-1$

  public static final String PROPERTY_STATION_STRATEGY = "stationStrategy"; //$NON-NLS-1$

  public static final String PROPERTY_STATION_PATTERN = "stationPattern"; //$NON-NLS-1$

  public static final String PROPERTY_STATION_PATTERN_ENABLED = "stationPatternEnabled"; //$NON-NLS-1$

  private final FileAndHistoryData m_inputDir = new FileAndHistoryData( "inputDir" ); //$NON-NLS-1$

  private CommandableWorkspace m_workspace;

  private WspmWaterBody m_water;

  private GUESS_STATION_STRATEGY m_stationStrategy = GUESS_STATION_STRATEGY.doNotGuess;

  private String m_srs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private String m_stationPattern = String.format( "<%s>", GuessStationPattern.TOKEN ); //$NON-NLS-1$

  void init( final IDialogSettings settings, final IFeatureSelection selection )
  {
    final IFeatureSelection featureSelection = selection;
    m_water = ImportProfileWizard.findWater( featureSelection );
    m_workspace = featureSelection.getWorkspace( m_water );

    if( settings == null )
      return;

    m_inputDir.init( settings );

    final String srs = settings.get( PROPERTY_SRS );
    if( !StringUtils.isBlank( srs ) )
      m_srs = srs;

    final String stategyName = settings.get( PROPERTY_STATION_STRATEGY );
    if( !StringUtils.isBlank( stategyName ) )
      m_stationStrategy = GUESS_STATION_STRATEGY.valueOf( stategyName );

    final String pattern = settings.get( PROPERTY_STATION_PATTERN );
    if( !StringUtils.isBlank( pattern ) )
      m_stationPattern = pattern;
  }

  void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_inputDir.storeSettings( settings );
    settings.put( PROPERTY_SRS, m_srs );
    settings.put( PROPERTY_STATION_STRATEGY, m_stationStrategy.name() );
    settings.put( PROPERTY_STATION_PATTERN, m_stationPattern );
  }

  public FileAndHistoryData getInputDir( )
  {
    return m_inputDir;
  }

  public WspmWaterBody getWater( )
  {
    return m_water;
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public String getSrs( )
  {
    return m_srs;
  }

  public void setSrs( final String srs )
  {
    final String oldValue = m_srs;

    m_srs = srs;

    firePropertyChange( PROPERTY_SRS, oldValue, srs );
  }

  public GUESS_STATION_STRATEGY getStationStrategy( )
  {
    return m_stationStrategy;
  }

  public void setStationStrategy( final GUESS_STATION_STRATEGY stationStrategy )
  {
    final GUESS_STATION_STRATEGY oldValue = m_stationStrategy;
    final boolean oldEnablement = getStationPatternEnabled();

    m_stationStrategy = stationStrategy;

    firePropertyChange( PROPERTY_STATION_STRATEGY, oldValue, stationStrategy );
    firePropertyChange( PROPERTY_STATION_PATTERN_ENABLED, oldEnablement, getStationPatternEnabled() );
  }

  public String getStationPattern( )
  {
    return m_stationPattern;
  }

  public void setStationPattern( final String stationPattern )
  {
    final String oldValue = m_stationPattern;

    m_stationPattern = stationPattern;

    firePropertyChange( PROPERTY_STATION_PATTERN, oldValue, stationPattern );
  }

  public boolean getStationPatternEnabled( )
  {
    return m_stationStrategy != GUESS_STATION_STRATEGY.doNotGuess;
  }
}