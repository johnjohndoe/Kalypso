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

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public class ChannelManager extends AbstractManager
{
  public static final int VIRTUALCHANNEL = 0;

  private static final int KMCHANNEL = 1;

  private static final String KMParameterpropName = "KMParameterMember";

  private final FeatureType m_virtualChannelFT;

  private final FeatureType m_kmChannelFT;

  private FeatureType m_kmParameterFT;

  public ChannelManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getChannelFormatURL() );
    m_virtualChannelFT = schema.getFeatureType( "VirtualChannel" );
    m_kmChannelFT = schema.getFeatureType( "KMChannel" );
    m_kmParameterFT = schema.getFeatureType( "KMParameter" );
  }

  /**
   * 
   * @see org.kalypso.convert.namodel.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection()
        .getInputStream() ) );// new FileReader( file ) );
    Feature fe = null;
    while( ( fe = readNextFeature( reader ) ) != null )
      result.add( fe );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;

    for( int i = 0; i <= 1; i++ )
    {
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( i + ": " + line );
      createProperties( propCollector, line, i );
    }
    FeatureProperty idProp = (FeatureProperty)propCollector.get( "inum" );
    FeatureProperty artProp = (FeatureProperty)propCollector.get( "iart" );
    int asciiID = Integer.parseInt( (String)idProp.getValue() );
    int art = Integer.parseInt( (String)artProp.getValue() );
    Feature feature;
    switch( art )
    {
    case VIRTUALCHANNEL:
      feature = getFeature( asciiID, m_virtualChannelFT );
      break;
    case KMCHANNEL:
      feature = getFeature( asciiID, m_kmChannelFT );
      line = reader.readLine();
      System.out.println( 2 + ": " + line );
      createProperties( propCollector, line, 2 );
      // parse kalinin-miljukov-parameter
      HashMap kmPropCollector = new HashMap();

      for( int i = 0; i < 5; i++ )
      {
        Feature kmParameterFeature = createFeature( m_kmParameterFT );
        line = reader.readLine();
        System.out.println( " km(" + i + "): " + line );
        createProperties( kmPropCollector, line, 3 );
        Collection collection = kmPropCollector.values();
        setParsedProperties( kmParameterFeature, collection );
        FeatureProperty kmProp = FeatureFactory.createFeatureProperty( KMParameterpropName,
            kmParameterFeature );
        feature.addProperty( kmProp );
      }
      break;
    default:
      throw new UnsupportedOperationException( "ChannelType " + art + " is not supported" );
    }
    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );        
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace )
  {
    Feature rootFeature = workspace.getRootFeature();
    Feature channelCol = (Feature)rootFeature.getProperty( "ChannelCollectionMember" );
    List channelList = (List)channelCol.getProperty( "channelMember" );
    Iterator iter = channelList.iterator();    
    while( iter.hasNext() )
    {
      Feature channelFE=(Feature)iter.next();
      if(asciiBuffer.writeFeature(channelFE))
        writeFeature(  asciiBuffer, channelFE);
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, Feature feature ) 
  {
    asciiBuffer.getChannelBuffer().append( toAscci( feature, 0 ) + "\n" );
    FeatureType ft = feature.getFeatureType();
    if( "VirtualChannel".equals( ft.getName() ) )
      asciiBuffer.getChannelBuffer().append( VIRTUALCHANNEL + "\n" );
    else if( "KMChannel".equals( ft.getName() ) )
    {
      asciiBuffer.getChannelBuffer().append( KMCHANNEL + "\n" );

      asciiBuffer.getChannelBuffer().append( toAscci( feature, 2 ) + "\n" );
      List kmFeatures = (List)feature.getProperty( KMParameterpropName );
      for( int i = 0; i < kmFeatures.size(); i++ )
      {
        Feature kmFE = (Feature)kmFeatures.get( i );
        asciiBuffer.getChannelBuffer().append( toAscci( kmFE, 3 ) + "\n" );
      }
    }
    else
      throw new UnsupportedOperationException( "can not write Feature to ascii"
          + feature.toString() );
  }

  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }
}