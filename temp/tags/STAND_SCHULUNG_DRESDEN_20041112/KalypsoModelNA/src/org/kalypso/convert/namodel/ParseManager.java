package org.kalypso.convert.namodel;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;

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

  public ParseManager( GMLSchema schema, NAConfiguration conf, CatchmentManager catchmentManager,
      ChannelManager channelManager, NetFileManager nodeManager )
  {
    m_conf = conf;
    m_catchmentManager = catchmentManager;
    m_channelManager = channelManager;
    m_nodeManager = nodeManager;
    m_schema = schema;
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
    System.out.println( "\n\n-----------------" );
    return naModellFe;

  }
}