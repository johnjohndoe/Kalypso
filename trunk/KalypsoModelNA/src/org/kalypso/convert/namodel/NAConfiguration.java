/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
/*
 * Created on Oct 7, 2004
 *  
 */
package org.kalypso.convert.namodel;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.kalypso.convert.namodel.manager.HydroHash;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class NAConfiguration
{
  private final File m_catchmentFile;

  private final File m_zftFile;

  private final URL m_ChannelFormatURL;

  private final URL m_catchmentFormatURL;

  private final File m_channelFile;

  private final File m_rhbFile;

  private final URL m_netFormatURL;

  private final URL m_rhbFormatURL;

  private final File m_netFile;

  private final File m_asciiBaseDir;

  private final File m_gmlBaseDir;

  private final IFeatureType m_nodeFT;

  private final IFeatureType m_catchmentFT;

  private final IFeatureType m_vChannelFT;

  private final IFeatureType m_stChannelFT;

  private final IFeatureType m_kmChannelFT;

  private final IFeatureType m_bodartFT;

  private final IFeatureType m_statNFT;

  private Date m_simulationForecast;

  private Date m_simulationStart;

  private Date m_simulationEnd;

  private String m_rootNodeId;

  private final File m_hydrotopFile;

  private final File m_bodentypFile;

  private final File m_bodenartFile;

  private final File m_schneeFile;

  private final File m_nutzungDir;

  private final File m_swaleAndTrenchFile;

  private final URL m_parameterFormatURL;

  private final URL m_hydrotopFormatURL;

  private final URL m_swaleAndTrenchFormatURL;

  private int m_minutesTimeStep = 60;

  private final IDManager m_idManager = new IDManager();

  private NaNodeResultProvider m_nodeResultProvider = null;

  private boolean m_pns;

  private Double m_annuality;

  private Double m_duration;

  private String m_precipitationForm;

  private URL m_zmlContext;

  private Date[] m_initialDates;

  private GMLWorkspace m_modelWorkspace = null;

  private GMLWorkspace m_parameterWorkspace = null;

  private GMLWorkspace m_hydrotopeWorkspace = null;

  private GMLWorkspace m_synthNWorkspace = null;

  private GMLWorkspace m_sudsWorkspace;

  private final static String PLC_LANDUSE_NAME_FORMAT = "PLC_%05d"; //$NON-NLS-1$

  private int m_plcLanduseCounter = 1;

  private final Map<String, String> m_landuseLongNamesMap = new HashMap<String, String>();

  private final File m_hydrotopMappingFile;

  private final List<String> m_hydrotopMapping = new ArrayList<String>();

  private final Map<String, List<Double>> m_suds2HydrotopMaxPercRateMap = new HashMap<String, List<Double>>();

  private final HydroHash m_hydroHash = new HydroHash();
  
  public NAConfiguration( final File asciiBaseDir )
  {
    this( asciiBaseDir, null );
  }

  public NAConfiguration( final File asciiBaseDir, final File gmlBaseDir )
  {
    m_asciiBaseDir = asciiBaseDir;
    m_gmlBaseDir = gmlBaseDir;

    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    final GMLSchema schema = getSchema( schemaCatalog, NaModelConstants.NS_NAMODELL );
    final GMLSchema paraSchema = getSchema( schemaCatalog, NaModelConstants.NS_NAPARAMETER );
    final GMLSchema synthNSchema = getSchema( schemaCatalog,  NaModelConstants.NS_SYNTHN );

    // featuretypes
    m_nodeFT = schema.getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    m_vChannelFT = schema.getFeatureType( NaModelConstants.V_CHANNEL_ELEMENT_FT );
    m_stChannelFT = schema.getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );
    m_kmChannelFT = schema.getFeatureType( NaModelConstants.KM_CHANNEL_ELEMENT_FT );
    m_catchmentFT = schema.getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    m_bodartFT = paraSchema.getFeatureType( NaModelConstants.PARA_SoilLayer_FT );
    m_statNFT = synthNSchema.getFeatureType( NaModelConstants.SYNTHN_STATN_FT );

    // formats:
    m_catchmentFormatURL = getClass().getResource( "formats/WernerCatchment.txt" ); //$NON-NLS-1$
    // kalypsoNa-sourcecode
    m_ChannelFormatURL = getClass().getResource( "formats/gerinne.txt" ); //$NON-NLS-1$
    m_netFormatURL = getClass().getResource( "formats/netzdatei.txt" ); //$NON-NLS-1$
    m_rhbFormatURL = getClass().getResource( "formats/JessicaRHB.txt" ); //$NON-NLS-1$
    m_hydrotopFormatURL = getClass().getResource( "formats/hydrotop.txt" ); //$NON-NLS-1$
    m_parameterFormatURL = getClass().getResource( "formats/parameter.txt" ); //$NON-NLS-1$
    m_swaleAndTrenchFormatURL = getClass().getResource( "formats/swaleAndTrench.txt" ); //$NON-NLS-1$
    // ASCII
    new File( asciiBaseDir, "inp.dat" ).mkdirs(); //$NON-NLS-1$
    new File( asciiBaseDir, "hydro.top" ).mkdirs(); //$NON-NLS-1$

    m_catchmentFile = new File( asciiBaseDir, "inp.dat/we_nat.geb" ); //$NON-NLS-1$
    m_zftFile = new File( asciiBaseDir, "inp.dat/we_nat.zft" ); //$NON-NLS-1$
    m_channelFile = new File( asciiBaseDir, "inp.dat/we_nat.ger" ); //$NON-NLS-1$
    m_netFile = new File( asciiBaseDir, "inp.dat/we_nat.ntz" ); //$NON-NLS-1$
    m_rhbFile = new File( asciiBaseDir, "inp.dat/we_nat.rhb" ); //$NON-NLS-1$
    m_nutzungDir = new File( asciiBaseDir, "hydro.top" ); //$NON-NLS-1$
    m_hydrotopFile = new File( asciiBaseDir, "inp.dat/we.hyd" ); //$NON-NLS-1$
    m_hydrotopMappingFile = new File( asciiBaseDir, "inp.dat/mapping.txt" ); //$NON-NLS-1$
    m_bodentypFile = new File( asciiBaseDir, "hydro.top/boden.dat" ); //$NON-NLS-1$
    m_bodenartFile = new File( asciiBaseDir, "hydro.top/bod_art.dat" ); //$NON-NLS-1$
    m_schneeFile = new File( asciiBaseDir, "hydro.top/snowtyp.dat" ); //$NON-NLS-1$
    m_swaleAndTrenchFile = new File( asciiBaseDir, "inp.dat/we_nat.mr" ); //$NON-NLS-1$
  }

  private GMLSchema getSchema( final GMLSchemaCatalog schemaCatalog, final String namespace )
  {
    try
    {
      return schemaCatalog.getSchema( namespace, (String) null );
    }
    catch( final GMLSchemaException e )
    {
      // will not happen
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Returns landuse name that is compatible with the calculation core; mappings are stored so once given ID is used
   * again if requested; for null gml names, the new ID is given without storing it
   */
  public final String getLanduseFeatureShortedName( final String featureName )
  {
    if( featureName != null && featureName.length() < 10 )
      return featureName;
    final String shortName = String.format( Locale.US, PLC_LANDUSE_NAME_FORMAT, m_plcLanduseCounter++ );
    System.out.println( "Created " + shortName + " for " + featureName ); //$NON-NLS-1$ //$NON-NLS-2$
    if( featureName == null )
      return shortName;
    final String string = m_landuseLongNamesMap.get( featureName );
    if( string == null )
    {
      m_landuseLongNamesMap.put( featureName, shortName );
      return shortName;
    }
    return string;
  }

  public URL getChannelFormatURL( )
  {
    return m_ChannelFormatURL;
  }

  public File getChannelFile( )
  {
    return m_channelFile;
  }

  public URL getCatchmentFormatURL( )
  {
    return m_catchmentFormatURL;
  }

  public File getCatchmentFile( )
  {
    return m_catchmentFile;
  }

  public File getZFTFile( )
  {
    return m_zftFile;
  }

  public URL getNetFormatURL( )
  {
    return m_netFormatURL;
  }

  public File getNetFile( )
  {
    return m_netFile;
  }

  public URL getRHBFormatURL( )
  {
    return m_rhbFormatURL;
  }

  public File getRHBFile( )
  {
    return m_rhbFile;
  }

  public File getAsciiBaseDir( )
  {
    return m_asciiBaseDir;
  }

  public File getGmlBaseDir( )
  {
    return m_gmlBaseDir;
  }

  public IFeatureType getNodeFT( )
  {
    return m_nodeFT;
  }

  public IFeatureType getCatchemtFT( )
  {
    return m_catchmentFT;
  }

  public IFeatureType getKmChannelFT( )
  {
    return m_kmChannelFT;
  }

  public IFeatureType getstatNFT( )
  {
    return m_statNFT;
  }

  public IFeatureType getVChannelFT( )
  {
    return m_vChannelFT;
  }

  public IFeatureType getStChannelFT( )
  {
    return m_stChannelFT;
  }

  public void setSimulationForecasetStart( final Date simulationForecast )
  {
    m_simulationForecast = simulationForecast;
  }

  public void setSimulationStart( final Date simulationStart )
  {
    m_simulationStart = simulationStart;
  }

  public void setSimulationEnd( final Date simulationEnd )
  {
    m_simulationEnd = simulationEnd;
  }

  public Date getSimulationStart( )
  {
    return m_simulationStart;
  }

  public Date getSimulationEnd( )
  {
    return m_simulationEnd;
  }

  public Date getSimulationForecastStart( )
  {
    return m_simulationForecast;
  }

  public String getRootNodeId( )
  {
    return m_rootNodeId;
  }

  public void setRootNodeID( final String rootNodeID )
  {
    m_rootNodeId = rootNodeID;
  }

  public File getHydrotopFile( )
  {
    return m_hydrotopFile;
  }

  public URL getHydrotopFormatURL( )
  {
    return m_hydrotopFormatURL;
  }

  public URL getParameterFormatURL( )
  {
    return m_parameterFormatURL;
  }

  public File getBodentypFile( )
  {
    return m_bodentypFile;
  }

  public File getBodenartFile( )
  {
    return m_bodenartFile;
  }

  public File getSchneeFile( )
  {
    return m_schneeFile;
  }

  public File getNutzungDir( )
  {
    return m_nutzungDir;
  }

  public File getSwaleAndTrenchFile( )
  {
    return m_swaleAndTrenchFile;
  }

  public IFeatureType getBodartFT( )
  {
    return m_bodartFT;
  }

  /**
   * @param minutesTimeStep
   */
  public void setMinutesOfTimeStep( final int minutesTimeStep )
  {
    m_minutesTimeStep = minutesTimeStep;
  }

  public void setUsePrecipitationForm( final boolean pns )
  {
    m_pns = pns;
  }

  public boolean isUsePrecipitationForm( )
  {
    return m_pns;
  }

  public int getMinutesOfTimeStep( )
  {
    return m_minutesTimeStep;
  }

  public IDManager getIdManager( )
  {
    return m_idManager;
  }

  /**
   * @param nodeResultProvider
   */
  public void setNodeResultProvider( final NaNodeResultProvider nodeResultProvider )
  {
    m_nodeResultProvider = nodeResultProvider;
  }

  public NaNodeResultProvider getNodeResultProvider( )
  {
    return m_nodeResultProvider;
  }

  public void setAnnuality( final Double annuality )
  {
    m_annuality = annuality;
  }

  public void setDuration( final Double duration )
  {
    m_duration = duration;
  }

  public void setForm( final String precipitationForm )
  {
    m_precipitationForm = precipitationForm;
  }

  public String getPrecipitationForm( )
  {
    return m_precipitationForm;
  }

  public Double getAnnuality( )
  {
    return m_annuality;
  }

  public Double getDuration( )
  {
    return m_duration;
  }

  public void setZMLContext( final URL zmlContext )
  {
    m_zmlContext = zmlContext;
  }

  public URL getZMLContext( )
  {
    return m_zmlContext;
  }

  public URL getSwaleAndTrenchFormatURL( )
  {
    return m_swaleAndTrenchFormatURL;
  }

  public void setInitalValues( final Date[] initialDates )
  {
    m_initialDates = initialDates;
  }

  public Date[] getInitialDates( )
  {
    return m_initialDates;
  }

  public void setModelWorkspace( final GMLWorkspace modelWorkspace )
  {
    m_modelWorkspace = modelWorkspace;
  }

  public GMLWorkspace getModelWorkspace( )
  {
    return m_modelWorkspace;
  }

  public void setParameterWorkspace( final GMLWorkspace parameterWorkspace )
  {
    m_parameterWorkspace = parameterWorkspace;
  }

  public GMLWorkspace getParameterWorkspace( )
  {
    return m_parameterWorkspace;
  }

  public void setHydrotopeWorkspace( final GMLWorkspace hydrotopeWorkspace )
  {
    m_hydrotopeWorkspace = hydrotopeWorkspace;
  }

  public GMLWorkspace getHydrotopeWorkspace( )
  {
    return m_hydrotopeWorkspace;
  }

  public void setSynthNWorkspace( final GMLWorkspace synthNWorkspace )
  {
    m_synthNWorkspace = synthNWorkspace;
  }

  public GMLWorkspace getSynthNWorkspace( )
  {
    return m_synthNWorkspace;
  }

  public void setSudsWorkspace( final GMLWorkspace sudsWorkspace )
  {
    m_sudsWorkspace = sudsWorkspace;
  }

  public GMLWorkspace getSudsWorkspace( )
  {
    return m_sudsWorkspace;
  }

  public File getHydrotopMappingFile( )
  {
    return m_hydrotopMappingFile;
  }

  public void addHydrotopMapping( final int catchmentAsciiID, final int hydrotopAsciiID, final Hydrotop hydrotop )
  {
    getHydrotopMapping().add( String.format( Locale.US, "%6d %6d   --->   [%s] \t%s", catchmentAsciiID, hydrotopAsciiID, hydrotop.getId(), hydrotop.getName() ) ); //$NON-NLS-1$
  }

  public List<String> getHydrotopMapping( )
  {
    return m_hydrotopMapping;
  }

  public void addSudsMaxPercRateMember( final String sudsFeatureID, final Double hydrotopMaxPerkolationRate )
  {
    List<Double> list = m_suds2HydrotopMaxPercRateMap.get( sudsFeatureID );
    if( list == null )
    {
      list = new ArrayList<Double>();
      m_suds2HydrotopMaxPercRateMap.put( sudsFeatureID, list );
    }
    if( hydrotopMaxPerkolationRate == null || Double.isNaN( hydrotopMaxPerkolationRate ) )
      return;
    list.add( hydrotopMaxPerkolationRate );
  }

  public Double getSudsAverageMaxPercRate( final String sudsFeatureID )
  {
    final List<Double> list = m_suds2HydrotopMaxPercRateMap.get( sudsFeatureID );
    if( list == null || list.size() == 0 )
      return Double.NaN;
    double average = 0.0;
    for( final double value : list )
      average += value;
    return average / list.size();
  }

  public HydroHash getHydroHash(  )
  {
    return m_hydroHash;
  }

}