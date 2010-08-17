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

import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.KMChannel;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.binding.model.VirtualChannel;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class NAConfiguration
{
  private final File m_catchmentFile;

  private final File m_zftFile;

  private final File m_channelFile;

  private final File m_rhbFile;

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

  private final File m_hydrotopFile;

  private final File m_bodentypFile;

  private final File m_bodenartFile;

  private final File m_schneeFile;

  private final File m_nutzungDir;

  private final File m_swaleAndTrenchFile;

  private final IDManager m_idManager = new IDManager();

  private URL m_zmlContext;

  private final File m_hydrotopMappingFile;

  private NaSimulationData m_simulationData = null;

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
    m_nodeFT = schema.getFeatureType( Node.FEATURE_NODE );
    m_vChannelFT = schema.getFeatureType( VirtualChannel.FEATURE_VIRTUAL_CHANNEL );
    m_stChannelFT = schema.getFeatureType( StorageChannel.FEATURE_STORAGE_CHANNEL );
    m_kmChannelFT = schema.getFeatureType( KMChannel.FEATURE_KM_CHANNEL );
    m_catchmentFT = schema.getFeatureType( Catchment.FEATURE_CATCHMENT );
    m_bodartFT = paraSchema.getFeatureType( NaModelConstants.PARA_SoilLayer_FT );
    m_statNFT = synthNSchema.getFeatureType( NaModelConstants.SYNTHN_STATN_FT );

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

  public void setSimulationData( final NaSimulationData simulationData )
  {
    m_simulationData = simulationData;
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

  public File getChannelFile( )
  {
    return m_channelFile;
  }

  public File getCatchmentFile( )
  {
    return m_catchmentFile;
  }

  public File getZFTFile( )
  {
    return m_zftFile;
  }

  public File getNetFile( )
  {
    return m_netFile;
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

  public File getHydrotopFile( )
  {
    return m_hydrotopFile;
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

  public IDManager getIdManager( )
  {
    return m_idManager;
  }

  public void setZMLContext( final URL zmlContext )
  {
    m_zmlContext = zmlContext;
  }

  public URL getZMLContext( )
  {
    return m_zmlContext;
  }

  public GMLWorkspace getModelWorkspace( )
  {
    return m_simulationData.getModelWorkspace();
  }

  public GMLWorkspace getParameterWorkspace( )
  {
    if( m_simulationData == null )
      return null;

    return m_simulationData.getParameterWorkspace();
  }

  public NAHydrotop getHydrotopeCollection( )
  {
    return m_simulationData.getHydrotopCollection();
  }

  public GMLWorkspace getSynthNWorkspace( )
  {
    return m_simulationData.getSynthNWorkspace();
  }

  public GMLWorkspace getSudsWorkspace( )
  {
    return m_simulationData.getSudsWorkspace();
  }

  public File getHydrotopMappingFile( )
  {
    return m_hydrotopMappingFile;
  }

  public NAControl getMetaControl( )
  {
    return m_simulationData.getMetaControl();
  }

}