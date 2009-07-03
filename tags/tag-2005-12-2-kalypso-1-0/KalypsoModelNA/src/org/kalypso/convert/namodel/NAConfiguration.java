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
import java.util.Date;

import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;

/**
 * @author doemming
 *  
 */
public class NAConfiguration
{

  //  private final URL m_schemaURL;

  private final File m_catchmentFile;

  private final File m_zftFile;

  private final URL m_ChannelFormatURL;

  private final URL m_catchmentFormatURL;

  private final File m_channelFile;

  private final File m_rhbFile;

  private final URL m_netFormatURL;

  private final URL m_rhbFormatURL;

  private final File m_netFile;

  private final URL m_controlSchemaURL;

  private final URL m_gmlModelURL;

  private final File m_asciiBaseDir;

  private final File m_gmlBaseDir;

  private final FeatureType m_nodeFT;

  private final FeatureType m_catchmentFT;

  private final FeatureType m_vChannelFT;

  private final FeatureType m_stChannelFT;

  private final FeatureType m_kmChannelFT;

  private final FeatureType m_bodartFT;

  private Date m_simulationForecast;

  private Date m_simulationStart;

  private Date m_simulationEnd;

  private String m_rootNodeId;

  //  private final URL m_metaSchemaURL;

  private final File m_hydrotopFile;

  private final File m_bodentypFile;

  private final File m_bodenartFile;

  private final File m_schneeFile;

  private final File m_nutzungDir;

  //  private final URL m_parameterSchemaURL;
  //
  //  private final URL m_hydrotopSchemaUrl;

  private final URL m_parameterFormatURL;

  private final URL m_hydrotopFormatURL;

  private int m_minutesTimeStep = 60;

  private final IDManager m_idManager = new IDManager();

  private String m_szenarioID = "";

  private NaNodeResultProvider m_nodeResultProvider=null;

  private NAConfiguration( File asciiBaseDir, File gmlBaseDir, URL modelURL ) throws Exception
  {
    m_asciiBaseDir = asciiBaseDir;
    m_gmlBaseDir = gmlBaseDir;
    m_gmlModelURL = modelURL;

    // schemas
    //    m_schemaURL = getClass().getResource( "schema/namodell.xsd" );
    //    m_metaSchemaURL = getClass().getResource( "schema/control.xsd" );
    //    m_parameterSchemaURL = getClass().getResource( "schema/parameter.xsd" );
    //    m_hydrotopSchemaUrl = getClass().getResource( "schema/hydrotop.xsd" );
    final GMLSchema schema = GMLSchemaCatalog.getSchema( NaModelConstants.NS_NAMODELL );
    final GMLSchema paraSchema = GMLSchemaCatalog.getSchema( NaModelConstants.NS_NAPARAMETER );

    // featuretypes
    m_nodeFT = schema.getFeatureType( "Node" );
    m_vChannelFT = schema.getFeatureType( "VirtualChannel" );
    m_stChannelFT = schema.getFeatureType( "StorageChannel" );
    m_kmChannelFT = schema.getFeatureType( "KMChannel" );
    m_catchmentFT = schema.getFeatureType( "Catchment" );
    m_bodartFT = paraSchema.getFeatureType( "Bodenart" );
    m_controlSchemaURL = getClass().getResource( "schema/nacontrol.xsd" );

    // formats:
    m_catchmentFormatURL = getClass().getResource( "formats/WernerCatchment.txt" );
    // TODO WernerCatchment und JessicaCatchment vergleichen mit
    // kalypsoNa-sourcecode
    //    m_catchmentFormatURL =
    // getClass().getResource("formats/JessicaCatchment.txt" );
    m_ChannelFormatURL = getClass().getResource( "formats/gerinne.txt" );
    m_netFormatURL = getClass().getResource( "formats/netzdatei.txt" );
    m_rhbFormatURL = getClass().getResource( "formats/JessicaRHB.txt" );
    m_hydrotopFormatURL = getClass().getResource( "formats/hydrotop.txt" );
    m_parameterFormatURL = getClass().getResource( "formats/parameter.txt" );

    // ASCII
    ( new File( asciiBaseDir, "inp.dat" ) ).mkdirs();
    ( new File( asciiBaseDir, "hydro.top" ) ).mkdirs();
    m_catchmentFile = new File( asciiBaseDir, "inp.dat/we_nat.geb" );
    m_zftFile = new File( asciiBaseDir, "inp.dat/we_nat.zft" );
    m_channelFile = new File( asciiBaseDir, "inp.dat/we_nat.ger" );
    m_netFile = new File( asciiBaseDir, "inp.dat/we_nat.ntz" );
    m_rhbFile = new File( asciiBaseDir, "inp.dat/we_nat.rhb" );
    m_nutzungDir = new File( asciiBaseDir, "hydro.top" );
    m_hydrotopFile = new File( asciiBaseDir, "inp.dat/we.hyd" );
    m_bodentypFile = new File( asciiBaseDir, "hydro.top/boden.dat" );
    m_bodenartFile = new File( asciiBaseDir, "hydro.top/bod_art.dat" );
    m_schneeFile = new File( asciiBaseDir, "hydro.top/snowtyp.dat" );

  }

  public static NAConfiguration getAscii2GmlConfiguration( File asciiBaseDir, File gmlBaseDir ) throws Exception
  {
    return new NAConfiguration( asciiBaseDir, gmlBaseDir, null );
  }

  public static NAConfiguration getGml2AsciiConfiguration( URL modelURL, File asciiBaseDir ) throws Exception
  {
    return new NAConfiguration( asciiBaseDir, null, modelURL );
  }

  //  public URL getSchemaURL()
  //  {
  //    return m_schemaURL;
  //  }

  public URL getChannelFormatURL()
  {
    return m_ChannelFormatURL;
  }

  public File getChannelFile()
  {
    return m_channelFile;
  }

  public URL getCatchmentFormatURL()
  {
    return m_catchmentFormatURL;
  }

  public File getCatchmentFile()
  {
    return m_catchmentFile;
  }

  public File getZFTFile()
  {
    return m_zftFile;
  }

  public URL getNetFormatURL()
  {
    return m_netFormatURL;
  }

  public File getNetFile()
  {
    return m_netFile;
  }

  public URL getRHBFormatURL()
  {
    return m_rhbFormatURL;
  }

  public File getRHBFile()
  {
    return m_rhbFile;
  }

  public URL getControlSchemaURL()
  {
    return m_controlSchemaURL;
  }

  public URL getGMLModelURL()
  {
    return m_gmlModelURL;
  }

  public File getAsciiBaseDir()
  {
    return m_asciiBaseDir;
  }

  public File getGmlBaseDir()
  {
    return m_gmlBaseDir;
  }

  public FeatureType getNodeFT()
  {
    return m_nodeFT;
  }

  public FeatureType getCatchemtFT()
  {
    return m_catchmentFT;
  }

  public FeatureType getKmChannelFT()
  {
    return m_kmChannelFT;
  }

  public FeatureType getVChannelFT()
  {
    return m_vChannelFT;
  }

  public FeatureType getStChannelFT()
  {
    return m_stChannelFT;
  }

  public void setSimulationForecasetStart( Date simulationForecast )
  {
    m_simulationForecast = simulationForecast;
  }

  public void setSimulationStart( Date simulationStart )
  {
    m_simulationStart = simulationStart;
  }

  public void setSimulationEnd( Date simulationEnd )
  {
    m_simulationEnd = simulationEnd;
  }

  public Date getSimulationStart()
  {
    return m_simulationStart;
  }

  public Date getSimulationEnd()
  {
    return m_simulationEnd;
  }

  public Date getSimulationForecastStart()
  {
    return m_simulationForecast;
  }

  public String getRootNodeId()
  {
    return m_rootNodeId;
  }

  public void setRootNodeID( String rootNodeID )
  {
    m_rootNodeId = rootNodeID;
  }

  //  public URL getMetaSchemaURL()
  //  {
  //    return m_metaSchemaURL;
  //  }

  public File getHydrotopFile()
  {
    return m_hydrotopFile;
  }

  //  public URL getParameterSchemaURL()
  //  {
  //    return m_parameterSchemaURL;
  //  }

  //  public URL getHydrotopSchemaUrl()
  //  {
  //    return m_hydrotopSchemaUrl;
  //  }

  public URL getHydrotopFormatURL()
  {
    return m_hydrotopFormatURL;
  }

  public URL getParameterFormatURL()
  {
    return m_parameterFormatURL;
  }

  public File getBodentypFile()
  {
    return m_bodentypFile;
  }

  public File getBodenartFile()
  {
    return m_bodenartFile;
  }

  public File getSchneeFile()
  {
    return m_schneeFile;
  }

  public File getNutzungDir()
  {
    return m_nutzungDir;
  }

  public FeatureType getBodartFT()
  {
    return m_bodartFT;
  }

  /**
   * @param minutesTimeStep
   */
  public void setMinutesOfTimeStep( int minutesTimeStep )
  {
    m_minutesTimeStep = minutesTimeStep;
  }

  public int getMinutesOfTimeStep()
  {
    return m_minutesTimeStep;
  }

  public IDManager getIdManager()
  {
    return m_idManager;
  }

  /**
   * All output timeseries must be marked with the szenario id
   * 
   * @param szenarioID
   */
  public void setSzenarioID( final String szenarioID )
  {
    m_szenarioID = szenarioID;
  }

  public String getScenarioID()
  {
    return m_szenarioID;
  }

  /**
   * @param nodeResultProvider
   */
  public void setNodeResultProvider( NaNodeResultProvider nodeResultProvider )
  {
    m_nodeResultProvider = nodeResultProvider;
  }

  public NaNodeResultProvider getNodeResultProvider()
  {
    return m_nodeResultProvider;
  }
}