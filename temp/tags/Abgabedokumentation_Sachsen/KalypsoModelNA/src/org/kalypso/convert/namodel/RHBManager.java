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
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypso.java.util.FortranFormatHelper;
import org.kalypso.ogc.gml.typehandler.DiagramProperty;

/**
 * @author huebsch
 */
public class RHBManager extends AbstractManager
{
  private final FeatureType m_storageChannelFT;

  final public NAConfiguration m_conf;

  public RHBManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getRHBFormatURL() );
    m_conf = conf;
    m_storageChannelFT = schema.getFeatureType( "StorageChannel" );
  }

  /**
   * 
   * @see org.kalypso.convert.namodel.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection()
        .getInputStream() ) );// new FileReader( file
    // ) );
    Feature fe = null;
    while( ( fe = readNextFeature( reader ) ) != null )
      result.add( fe );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;

    //JessicaRHB.txt

    for( int i = 0; i <= 2; i++ )
    {
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( i + ": " + line );
      createProperties( propCollector, line, i );
    }
    FeatureProperty idProp = (FeatureProperty)propCollector.get( "inum" );
    int asciiID = Integer.parseInt( (String)idProp.getValue() );
    final Feature rhbStrangFE = getFeature( asciiID, m_conf.getStChannelFT() );
    final FeatureProperty iknotProp = (FeatureProperty)propCollector.get( "iknot" );
    int iknotNr = Integer.parseInt( (String)iknotProp.getValue() );
    if( iknotNr > 0 )
    {
      final Feature knotFE = getFeature( iknotNr, m_conf.getNodeFT() );
      final FeatureProperty iknotNodeMember = FeatureFactory.createFeatureProperty(
          "iknotNodeMember", knotFE.getId() );
      rhbStrangFE.setProperty( iknotNodeMember );
    }
    FeatureProperty jevProp = (FeatureProperty)propCollector.get( "jev" );
    int jev = Integer.parseInt( (String)jevProp.getValue() );
    final DiagramProperty diagram = new DiagramProperty();
    for( int i = 0; i < jev; i++ )
    {
      line = reader.readLine();
      System.out.println( i + ": " + line );
      final HashMap map = FortranFormatHelper.scanf( getAsciiFormats()[3], line );
      Double hv = new Double( (String)map.get( "hv" ) );
      Double vs = new Double( (String)map.get( "vs" ) );
      Double qd = new Double( (String)map.get( "qd" ) );
      diagram.addValue( hv, vs, qd );
    }
    FeatureProperty diagramProp = FeatureFactory.createFeatureProperty( "hvvsqd", diagram );
    rhbStrangFE.setProperty( diagramProp );
    line = reader.readLine();
    System.out.println( "4: " + line );
    createProperties( propCollector, line, 4 );

    final Feature feature = getFeature( asciiID, m_storageChannelFT );

    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace )
  {
  // not needed. gerinnemanager writes rhbs
  }

  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }
}