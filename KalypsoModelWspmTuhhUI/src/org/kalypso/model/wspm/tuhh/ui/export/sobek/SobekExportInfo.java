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
import org.eclipse.jface.dialogs.IMessageProvider;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateDirectory;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserGroup.FileChangedListener;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.IProfilePatternData;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternData;
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.IFlowZoneType;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.LeftBank;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.LeftForeland;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.MainChannel;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.RightBank;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.flowzones.RightForeland;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * Basic data for all sobek export operations.
 * 
 * @author Gernot Belger
 */
public class SobekExportInfo
{
  private static final IFlowZoneType[] ALL_ROUGHNESS_ZONES = new IFlowZoneType[] { new LeftForeland(), new LeftBank(), new MainChannel(), new RightBank(), new RightForeland() };

  private static final String SETTING_ROUGHNESS = "roughnessId"; //$NON-NLS-1$

  private static final String SETTING_ROUGHNESS_ZONES = "zones"; //$NON-NLS-1$

  private static final String SETTINGS_PROFILE_EXPORT_FLOW_ZONE = "profileExportFlowZone"; //$NON-NLS-1$

  private static final String SETTINGS_EXPORT_BUILDINGS = "exportBuildings"; //$NON-NLS-1$

  private static final String SETTINGS_ID_PATTERN = "idPattern"; //$NON-NLS-1$

  private static final String SETTINGS_NAME_PATTERN = "namePattern"; //$NON-NLS-1$

  private static final String SETTINGS_BUILDINGS_SUFFIX = "buildingsSuffix"; //$NON-NLS-1$

  private final FileChooserGroup m_dirChooser;

  private String m_idPattern = "<Name>"; //$NON-NLS-1$

  private String m_idSuffix = "_building"; //$NON-NLS-1$

  private String m_namePattern = "<Name>"; //$NON-NLS-1$

  private String m_flowZone = IWspmTuhhConstants.MARKER_TYP_BORDVOLL;

  private boolean m_exportBuildings = true;

  private String m_roughnessId = IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS;

  private IFlowZoneType[] m_roughnessZoneTypes = new IFlowZoneType[0];

  private final SobekProfileExportFileChooserPage m_page;

  private final IDialogSettings m_settings;

  private IProfileFeature[] m_profiles;

  public SobekExportInfo( final SobekProfileExportFileChooserPage page, final IDialogSettings settings )
  {
    m_page = page;

    m_settings = settings;

    final IFileChooserDelegate dirDelegate = new FileChooserDelegateDirectory();
    m_dirChooser = new FileChooserGroup( dirDelegate );

    m_dirChooser.addFileChangedListener( new FileChangedListener()
    {
      @Override
      public void fileChanged( final File file )
      {
        page.validatePage();
      }
    } );
  }

  public String getIdPattern( )
  {
    return m_idPattern;
  }

  public String getIdSuffix( )
  {
    return m_idSuffix;
  }

  public String getNamePattern( )
  {
    return m_namePattern;
  }

  public FileChooserGroup getDirChooser( )
  {
    return m_dirChooser;
  }

  public File getTargetDir( )
  {
    return m_dirChooser.getFile();
  }

  public String getFlowZone( )
  {
    return m_flowZone;
  }

  public boolean getExportBuildings( )
  {
    return m_exportBuildings;
  }

  public String getRoughnessID( )
  {
    return m_roughnessId;
  }

  public IFlowZoneType[] getRoughnessZoneTypes( )
  {
    return m_roughnessZoneTypes;
  }

  public IFlowZoneType[] getAllRoughnessZones( )
  {
    return ALL_ROUGHNESS_ZONES;
  }

  public IProfileFeature[] getProfiles( )
  {
    return m_profiles;
  }

  public void setIdPattern( final String idPattern )
  {
    m_idPattern = idPattern;

    saveSettings();

    updateMessage();
  }

  public void setIdSuffix( final String idSuffix )
  {
    m_idSuffix = idSuffix;

    saveSettings();

    updateMessage();
  }

  public void setNamePattern( final String namePattern )
  {
    m_namePattern = namePattern;

    saveSettings();

    updateMessage();
  }

  public void setExportBridges( final boolean selection )
  {
    m_exportBuildings = selection;

    saveSettings();
  }

  public void setFlowZone( final String flowZone )
  {
    m_flowZone = flowZone;

    saveSettings();
  }

  public void setRoughnessID( final String roughnessId )
  {
    m_roughnessId = roughnessId;

    saveSettings();
  }

  public void setRoughnessZoneTypes( final IFlowZoneType[] roughnessZoneTypes )
  {
    m_roughnessZoneTypes = roughnessZoneTypes;

    saveSettings();
  }

  public void setProfiles( final IProfileFeature[] profiles )
  {
    m_profiles = profiles;
  }

  public void readSettings( )
  {
    if( m_settings == null )
      return;

    final String idPattern = m_settings.get( SETTINGS_ID_PATTERN );
    if( idPattern != null )
      m_idPattern = idPattern;

    final String namePattern = m_settings.get( SETTINGS_NAME_PATTERN );
    if( namePattern != null )
      m_namePattern = namePattern;

    final String idSuffix = m_settings.get( SETTINGS_BUILDINGS_SUFFIX );
    if( idSuffix != null )
      m_idSuffix = idSuffix;

    /* Invert flag for better default behavior (we want true, but default emtpy settings is false) */
    m_exportBuildings = !m_settings.getBoolean( SETTINGS_EXPORT_BUILDINGS );

    final String settingsZone = m_settings.get( SETTINGS_PROFILE_EXPORT_FLOW_ZONE );
    if( settingsZone != null )
    {
      final String[] markerComponents = getMarkerComponents();
      for( final String knownComponent : markerComponents )
      {
        final String id = knownComponent;
        if( settingsZone.equals( id ) )
        {
          m_flowZone = settingsZone;
          break;
        }
      }
    }

    final String savedRoughness = m_settings.get( SETTING_ROUGHNESS );
    if( !StringUtils.isBlank( savedRoughness ) )
      m_roughnessId = savedRoughness;

    final String[] savedZones = m_settings.getArray( SETTING_ROUGHNESS_ZONES );
    if( savedZones != null )
    {
      final Collection<IFlowZoneType> zoneTypes = new ArrayList<IFlowZoneType>( savedZones.length );
      for( final String className : savedZones )
      {
        final IFlowZoneType zone = findZone( className );
        if( zone != null )
          zoneTypes.add( zone );
      }
      m_roughnessZoneTypes = zoneTypes.toArray( new IFlowZoneType[zoneTypes.size()] );
    }
  }

  private IFlowZoneType findZone( final String className )
  {
    for( final IFlowZoneType zoneType : ALL_ROUGHNESS_ZONES )
    {
      if( className.equals( zoneType.getClass().getName() ) )
        return zoneType;
    }

    return null;
  }

  public void saveSettings( )
  {
    if( m_settings == null )
      return;

    m_settings.put( SETTINGS_BUILDINGS_SUFFIX, m_idSuffix );
    m_settings.put( SETTINGS_ID_PATTERN, m_idPattern );
    m_settings.put( SETTINGS_NAME_PATTERN, m_namePattern );
    m_settings.put( SETTINGS_PROFILE_EXPORT_FLOW_ZONE, m_flowZone );
    m_settings.put( SETTING_ROUGHNESS, m_roughnessId );

    final String[] zoneNames = new String[m_roughnessZoneTypes.length];
    for( int i = 0; i < zoneNames.length; i++ )
      zoneNames[i] = m_roughnessZoneTypes[i].getClass().getName();
    m_settings.put( SETTING_ROUGHNESS_ZONES, zoneNames );
  }

  public IMessageProvider validate( )
  {
    final File exportDir = m_dirChooser.getFile();
    if( exportDir == null )
      return new MessageProvider( "Export directory not selected", IMessageProvider.WARNING ); //$NON-NLS-1$

    if( !exportDir.isDirectory() )
      return new MessageProvider( "Export directory is not a valid directory path", IMessageProvider.WARNING ); //$NON-NLS-1$

    if( StringUtils.isBlank( m_idPattern ) )
      return new MessageProvider( Messages.getString( "SobekProfileExportFileChooserPage_10" ), IMessageProvider.WARNING ); //$NON-NLS-1$

    if( StringUtils.isBlank( m_namePattern ) )
      return new MessageProvider( "Bitte geben Sie ein das Muster f¸r den Namen ein", IMessageProvider.WARNING );

    if( StringUtils.isBlank( m_idSuffix ) )
      return new MessageProvider( Messages.getString( "SobekProfileExportFileChooserPage.5" ), IMessageProvider.WARNING ); //$NON-NLS-1$

    return null;
  }

  public String getStructID( final IProfileFeature profileFeature )
  {
    final IProfil profil = profileFeature.getProfil();
    final String pattern = getIdPattern() + getIdSuffix();
    final IProfilePatternData data = new ProfilePatternData( profileFeature, profil, null );
    return ProfilePatternInputReplacer.getINSTANCE().replaceTokens( pattern, data );
  }

  public String getID( final IProfileFeature profileFeature )
  {
    final IProfil profil = profileFeature.getProfil();
    final IProfilePatternData data = new ProfilePatternData( profileFeature, profil, null );

    return ProfilePatternInputReplacer.getINSTANCE().replaceTokens( getIdPattern(), data );
  }

  public String getName( final IProfileFeature profileFeature )
  {
    final IProfil profil = profileFeature.getProfil();
    final IProfilePatternData data = new ProfilePatternData( profileFeature, profil, null );

    return ProfilePatternInputReplacer.getINSTANCE().replaceTokens( getNamePattern(), data );
  }

  public String[] getMarkerComponents( )
  {
    final Collection<String> markers = new ArrayList<String>();

    markers.add( StringUtils.EMPTY );
    markers.add( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    markers.add( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    markers.add( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    return markers.toArray( new String[markers.size()] );
  }

  private void updateMessage( )
  {
    if( m_page != null )
      m_page.updateMessage();
  }

  public void setTargetDir( final File targetDir )
  {
    m_dirChooser.setFile( targetDir );
  }

}
