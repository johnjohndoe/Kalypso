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
package org.kalypso.convert.namodel;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public class ParseManager
{
  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_channelManager;

  private final GMLSchema m_schema;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;
  
  private final RHBManager m_rhbManager;

  public ParseManager( GMLSchema schema, NAConfiguration conf, CatchmentManager catchmentManager,
      ChannelManager channelManager, NetFileManager nodeManager, RHBManager rhbManager )
  {
    m_conf = conf;
    m_catchmentManager = catchmentManager;
    m_channelManager = channelManager;
    m_nodeManager = nodeManager;
    m_schema = schema;
    m_rhbManager = rhbManager;
  }

  public Feature asciiToFeature() throws Exception, Exception
  {
    ModelManager modelManager = new ModelManager();
    // get all FeatureTypes...
    FeatureType naModellFT = m_schema.getFeatureType( "NaModell" );
    FeatureType catchmentCollectionFT = m_schema.getFeatureType( "CatchmentCollection" );
    FeatureType channelCollectionFT = m_schema.getFeatureType( "ChannelCollection" );
    FeatureType nodeCollectionFT = m_schema.getFeatureType( "NodeCollection" );
    // create all Features (and FeatureCollections)
    Feature naModellFe = modelManager.createFeature( naModellFT );
    Feature catchmentCollectionFe = modelManager.createFeature( catchmentCollectionFT );
    Feature channelCollectionFe = modelManager.createFeature( channelCollectionFT );
    Feature nodeCollectionFe = modelManager.createFeature( nodeCollectionFT );

    // complete Feature NaModell
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "CatchmentCollectionMember",
        catchmentCollectionFe );
    naModellFe.setProperty( prop );

    prop = FeatureFactory.createFeatureProperty( "ChannelCollectionMember", channelCollectionFe );
    naModellFe.setProperty( prop );

    prop = FeatureFactory.createFeatureProperty( "NodeCollectionMember", nodeCollectionFe );
    naModellFe.setProperty( prop );

    //complete Feature CatchmentCollection
    Feature[] features = m_catchmentManager.parseFile( m_conf.getCatchmentFile().toURL() );
    for( int i = 0; i < features.length; i++ )
    {
      Feature catchmentFE = features[i];
      prop = FeatureFactory.createFeatureProperty( "catchmentMember", catchmentFE );
      catchmentCollectionFe.addProperty( prop );
    }

    //complete Features of ChannelCollections
    features = m_channelManager.parseFile( m_conf.getChannelFile().toURL() );
    for( int i = 0; i < features.length; i++ )
    {
      Feature channelFE = features[i];
      prop = FeatureFactory.createFeatureProperty( "channelMember", channelFE );
      channelCollectionFe.addProperty( prop );

    }

    //complete Feature NodeCollection
    features = m_nodeManager.parseFile( m_conf.getNetFile().toURL() );
    for( int i = 0; i < features.length; i++ )
    {
      Feature nodeFE = features[i];
      prop = FeatureFactory.createFeatureProperty( "nodeMember", nodeFE );
      nodeCollectionFe.addProperty( prop );
    }
    
    //complete Features of StorageChannel
    features = m_rhbManager.parseFile( m_conf.getRHBFile().toURL() );
    

    System.out.println( "\n\n-----------------" );
    return naModellFe;

  }
}