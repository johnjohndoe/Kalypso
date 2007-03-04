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
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileFilter;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class ParseManager
{
  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_channelManager;

  private final GMLSchema m_schema;

  private final GMLSchema m_paraSchema;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;

  private final RHBManager m_rhbManager;

  private final BodenartManager m_bodartManager;

  private final BodentypManager m_bodtypManager;

  private final NutzungManager m_nutzManager;

  private final SchneeManager m_schneeManager;

  private final IdleLanduseManager m_idleLanduseManager;

  public ParseManager( GMLSchema schema, GMLSchema paraSchema, NAConfiguration conf, CatchmentManager catchmentManager, ChannelManager channelManager, NetFileManager nodeManager, RHBManager rhbManager, BodenartManager bodartManager, BodentypManager bodtypManager, NutzungManager nutzManager, SchneeManager schneeManager, IdleLanduseManager idleLanduseManager )
  {
    m_conf = conf;
    m_catchmentManager = catchmentManager;
    m_channelManager = channelManager;
    m_nodeManager = nodeManager;
    m_schema = schema;
    m_paraSchema = paraSchema;
    m_rhbManager = rhbManager;
    m_bodartManager = bodartManager;
    m_bodtypManager = bodtypManager;
    m_nutzManager = nutzManager;
    m_schneeManager = schneeManager;
    m_idleLanduseManager = idleLanduseManager;
  }

  public Feature modelAsciiToFeature( ) throws Exception, Exception
  {
    ModelManager modelManager = new ModelManager();
    // get all FeatureTypes...
    IFeatureType naModellFT = m_schema.getFeatureType( "NaModell" );
    IFeatureType catchmentCollectionFT = m_schema.getFeatureType( "CatchmentCollection" );
    IFeatureType channelCollectionFT = m_schema.getFeatureType( "ChannelCollection" );
    IFeatureType nodeCollectionFT = m_schema.getFeatureType( "NodeCollection" );

    // create all Features (and FeatureCollections)
    Feature naModellFe = modelManager.createFeature( naModellFT );
    Feature catchmentCollectionFe = modelManager.createFeature( catchmentCollectionFT );
    Feature channelCollectionFe = modelManager.createFeature( channelCollectionFT );
    Feature nodeCollectionFe = modelManager.createFeature( nodeCollectionFT );

    // complete Feature NaModell

    // final FeatureProperty prop = FeatureFactory.createFeatureProperty( "CatchmentCollectionMember",
    // catchmentCollectionFe );
    naModellFe.setProperty( "CatchmentCollectionMember", catchmentCollectionFe );

    // prop = FeatureFactory.createFeatureProperty( "ChannelCollectionMember", channelCollectionFe );
    naModellFe.setProperty( "ChannelCollectionMember", channelCollectionFe );

    // prop = FeatureFactory.createFeatureProperty( "NodeCollectionMember", nodeCollectionFe );
    naModellFe.setProperty( "NodeCollectionMember", nodeCollectionFe );

    // complete Feature CatchmentCollection
    final IPropertyType catchmentMemberPT = catchmentCollectionFe.getFeatureType().getProperty( "catchmentMember" );
    Feature[] features = m_catchmentManager.parseFile( m_conf.getCatchmentFile().toURL() );
    for( int i = 0; i < features.length; i++ )
      FeatureHelper.addProperty( catchmentCollectionFe, catchmentMemberPT, features[i] );

    // complete Features of ChannelCollections
    final IPropertyType channelMemberPT = channelCollectionFe.getFeatureType().getProperty( "channelMember" );
    features = m_channelManager.parseFile( m_conf.getChannelFile().toURL() );
    for( int i = 0; i < features.length; i++ )
      FeatureHelper.addProperty( channelCollectionFe, channelMemberPT, features[i] );

    // complete Feature NodeCollection
    final IPropertyType nodeMemberPT = nodeCollectionFT.getProperty( "nodeMember" );
    features = m_nodeManager.parseFile( m_conf.getNetFile().toURL() );
    for( int i = 0; i < features.length; i++ )
      FeatureHelper.addProperty( nodeCollectionFe, nodeMemberPT, features[i] );

    // complete Features of StorageChannel
    // features = m_rhbManager.parseFile( m_conf.getRHBFile().toURL() );

    System.out.println( "\n\n-----------------" );
    return naModellFe;
  }

  public Feature parameterAsciiToFeature( ) throws Exception, Exception
  {
    ModelManager modelManager = new ModelManager();
    // get all FeatureTypes...
    IFeatureType naParaFT = m_paraSchema.getFeatureType( "Parameter" );
    

    // create all Features (and FeatureCollections)
    Feature naParaFe = modelManager.createFeature( naParaFT );
    Feature[] features;
    IPropertyType soilLayerMemberPT = naParaFT.getProperty( "soilLayerMember" );
    IPropertyType soiltypeMemberPT = naParaFT.getProperty( "soiltypeMember" );

    // complete Feature soilLayerMember
    try
    {
      features = m_bodartManager.parseFile( m_conf.getBodenartFile().toURL() );
      for( int i = 0; i < features.length; i++ )
        FeatureHelper.addProperty( naParaFe, soilLayerMemberPT, features[i] );
    }
    catch( Exception e )
    {
      System.out.println( "keine Datei boden_art.dat vorhanden" );
    }

    // complete Feature soiltypeMember
    features = m_bodtypManager.parseFile( m_conf.getBodentypFile().toURL() );
    for( int i = 0; i < features.length; i++ )
      FeatureHelper.addProperty( naParaFe, soiltypeMemberPT, features[i] );

    // complete Feature idealLandUseMember
    File nutzungDir = m_conf.getNutzungDir();
    FileFilter filter = FileFilterUtils.suffixFileFilter( ".nuz" );
    File nutzFiles[] = nutzungDir.listFiles( filter );
    final IPropertyType idealLandUseMemberRT = naParaFT.getProperty( "idealLandUseMember" );
    for( int i = 0; i < nutzFiles.length; i++ )
    {
      // es kommt pro file immer nur ein feature zurück
      System.out.println( "Nutzungsdatei: " + nutzFiles[i].toURL().toString() );
      features = m_idleLanduseManager.parseFile( nutzFiles[i].toURL() );
      for( int f = 0; f < features.length; f++ )
        FeatureHelper.addProperty( naParaFe, idealLandUseMemberRT, features[f] );
    }
    final IPropertyType landuseMemberRT = naParaFT.getProperty( "landuseMember" );
    // complete Feature landuseMember
    for( int i = 0; i < nutzFiles.length; i++ )
    {
      // es kommt pro file immer nur ein feature zurück
      System.out.println( "Nutzungsdatei: " + nutzFiles[i].toURL().toString() );
      features = m_nutzManager.parseFile( nutzFiles[i].toURL() );
      for( int f = 0; f < features.length; f++ )
        FeatureHelper.addProperty( naParaFe, landuseMemberRT, features[f] );
    }
    System.out.println( "---------Es wurden " + nutzFiles.length + " Nutzungsdateien eingelesen" );
    
    
    final IPropertyType sealingMemberRT = naParaFT.getProperty( "sealingMember" );
    URL csvsealingURL = new File( nutzungDir, "Klassen_Sealing_KRUECK2007.csv" ).toURL();
    features=m_idleLanduseManager.parseSealingFilecsv(csvsealingURL);
    for( int f = 0; f < features.length; f++ )
      FeatureHelper.addProperty( naParaFe, sealingMemberRT, features[f] );

    
    URL csvURL = new File( nutzungDir, "Klassen_KRUECK2007.csv" ).toURL();
    features=m_idleLanduseManager.parseFilecsv(csvURL);
    for( int f = 0; f < features.length; f++ )
      FeatureHelper.addProperty( naParaFe, landuseMemberRT, features[f] );
    
    final IPropertyType snowMemberRT = naParaFT.getProperty( "snowMember" );
    // complete Feature snowMember
    try
    {
      features = m_schneeManager.parseFile( m_conf.getSchneeFile().toURL() );
      for( int i = 0; i < features.length; i++ )
        FeatureHelper.addProperty( naParaFe, snowMemberRT, features[i] );
    }
    catch( Exception e )
    {
      System.out.println( "keine Datei snowtyp.dat vorhanden" );
    }

    System.out.println( "\n\n-----------------" );
    return naParaFe;
  }
}