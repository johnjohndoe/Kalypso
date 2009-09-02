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

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author huebsch
 */
public class RHBManager extends AbstractManager
{
  private final IFeatureType m_storageChannelFT;

  final public NAConfiguration m_conf;

  public RHBManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getRHBFormatURL() );
    m_conf = conf;
    m_storageChannelFT = schema.getFeatureType( NaModelConstants.STORAGE_CHANNEL_ELEMENT_FT );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {
    List<Feature> result = new ArrayList<Feature>();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );
    Feature fe = null;
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;

    // JessicaRHB.txt
    for( int i = 0; i <= 2; i++ )
    {
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( i + ": " + line ); //$NON-NLS-1$
      createProperties( propCollector, line, i );
    }
    int asciiID = Integer.parseInt( propCollector.get( "inum" ) ); //$NON-NLS-1$
    final Feature rhbStrangFE = getFeature( asciiID, m_conf.getStChannelFT() );
    int iknotNr = Integer.parseInt( propCollector.get( "iknot" ) ); //$NON-NLS-1$
    if( iknotNr > 0 )
    {
      final Feature knotFE = getFeature( iknotNr, m_conf.getNodeFT() );
      rhbStrangFE.setProperty( NaModelConstants.IKNOT_MEMBER_PROP, knotFE.getId() );
    }
//    int jev = Integer.parseInt( propCollector.get( "jev" ) );
    // TODO: old Code - remove diagramm and add handling with new zmlinline typehandler
    // final DiagramProperty diagram = new DiagramProperty();
    // for( int i = 0; i < jev; i++ )
    // {
    // line = reader.readLine();
    // System.out.println( i + ": " + line );
    // final HashMap map = FortranFormatHelper.scanf( getAsciiFormats()[3], line );
    // Double hv = new Double( (String) map.get( "hv" ) );
    // Double vs = new Double( (String) map.get( "vs" ) );
    // Double qd = new Double( (String) map.get( "qd" ) );
    // diagram.addValue( hv, vs, qd );
    // }
    // rhbStrangFE.setProperty( "hvvsqd", diagram );
    line = reader.readLine();
    System.out.println( "4: " + line ); //$NON-NLS-1$
    createProperties( propCollector, line, 4 );

    final Feature feature = getFeature( asciiID, m_storageChannelFT );

    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  @Override
  public String mapID( int id, IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }
}