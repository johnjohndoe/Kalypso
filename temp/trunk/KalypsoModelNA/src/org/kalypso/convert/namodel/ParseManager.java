package org.kalypso.convert.namodel;

import java.io.File;
import java.io.IOException;

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

  public ParseManager( GMLSchema schema, CatchmentManager catchmentManager,
      ChannelManager channelManager )
  {
    m_catchmentManager = catchmentManager;
    m_channelManager = channelManager;
    m_schema = schema;
  }

  public Feature asciiToFeature() throws IOException
  {
  
    // get all FeatureTypes...
    FeatureType naModellFT = m_schema.getFeatureType( "NaModell" );
    FeatureType catchmentCollectionFT = m_schema.getFeatureType( "CatchmentCollection" );
    FeatureType virtualChannelCollectionFT = m_schema.getFeatureType( "VirtualChannelCollection" );
    FeatureType kmChannelCollectionFT = m_schema.getFeatureType( "KMChannelCollection" );
    // create all Features (and FeatureCollections)
    Feature naModellFe = FeatureFactory.createFeature("1", naModellFT);
    Feature catchmentCollectionFe = FeatureFactory.createFeature( "2",catchmentCollectionFT);
    Feature virtualChannelCollectionFe = FeatureFactory.createFeature("3", virtualChannelCollectionFT);
    Feature kmChannelCollectionFe = FeatureFactory.createFeature( "4",kmChannelCollectionFT);

    // complete Feature NaModell
    FeatureProperty prop = FeatureFactory.createFeatureProperty( "CatchmentCollectionMember",
        catchmentCollectionFe );
    naModellFe.setProperty( prop );

    prop = FeatureFactory.createFeatureProperty( "VirtualChannelCollectionMember",
        virtualChannelCollectionFe );
    naModellFe.setProperty( prop );

    prop = FeatureFactory
        .createFeatureProperty( "KMChannelCollectionMember", kmChannelCollectionFe );
    naModellFe.setProperty( prop );

    //complete Feature CatchmentCollection
    Feature[] features = m_catchmentManager.parseFile( new File( "data/inp.dat/we_nat.geb" ) );
    for( int i = 0; i < features.length; i++ )
    {
      Feature catchmentFE = features[i];
      prop = FeatureFactory.createFeatureProperty( "catchmentMember", catchmentFE );
      catchmentCollectionFe.addProperty( prop );
    }

    //complete Features of ChannelCollections
    features = m_channelManager.parseFile( new File( "data/inp.dat/we_nat.ger" ) );
    for( int i = 0; i < features.length; i++ )
    {
      Feature channelFE = features[i];
      FeatureType ft = channelFE.getFeatureType();
      if( "VirtualChannel".equals( ft.getName() ) )
      {
        prop = FeatureFactory.createFeatureProperty( "virtualChannelMember", channelFE );
        virtualChannelCollectionFe.addProperty( prop );  
      }
      else if( "KMChannel".equals( ft.getName() ) )
      {
        prop = FeatureFactory.createFeatureProperty( "kmChannelMember", channelFE );
        kmChannelCollectionFe.addProperty( prop );
      }
      else
        throw new UnsupportedOperationException( "channel must be of virtual- or km-type not"+ft.getName() );
 
    }
    System.out.println( "\n\n-----------------" );
    return naModellFe;
//    ( (ExtFeature_Impl)naModellFe ).debugOut( 0 );

  }
}