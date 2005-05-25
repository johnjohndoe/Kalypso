package org.kalypso.convert.namodel;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

public class BodenartManager extends AbstractManager
{

  private final NAConfiguration m_conf;

  private final FeatureType m_bodenartFT;

  /*
   * 
   * @author huebsch
   */
  public BodenartManager( GMLSchema schema, GMLSchema hydrotopSchema, GMLSchema parameterSchema,
      NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    //    m_crs = crs;
    m_conf = conf;

    m_bodenartFT = parameterSchema.getFeatureType( "Bodenart" );
  }

  /**
   * @see org.kalypso.convert.namodel.AbstractManager#mapID(int,
   *      org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, FeatureType ft )
  {
    throw new UnsupportedOperationException(
        " bodenartManager does not support int-ID mapping. (not necessary) " );
  }

  /**
   * @see org.kalypso.convert.namodel.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {

    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection()
        .getInputStream() ) );// new FileReader( file
    // ) );
    Feature fe = null;
    //  3 Kommentarzeilen
    for( int i = 0; i <= 2; i++ )
    {
      String line;
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( reader.getLineNumber() + ": " + line );
    }
    while( ( fe = readNextFeature( reader ) ) != null )
      result.add( fe );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;
    // 6
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );
    createProperties( propCollector, line, 6 );

    //  generate id:
    FeatureProperty prop = (FeatureProperty)propCollector.get( "typchar" );
    String asciiStringId = (String)prop.getValue();
    final Feature feature = getFeature( asciiStringId, m_bodenartFT );

    // continue reading

    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    return feature;
  }

  /**
   * @see org.kalypso.convert.namodel.AbstractManager#getFeature(int,
   *      org.kalypsodeegree.model.feature.FeatureType)
   */
  public Feature getFeature( int asciiID, FeatureType ft )
  {
    throw new UnsupportedOperationException(
        " bodenartManager does not support int-ID mapping. (not necessary) " );
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace ) throws Exception
  {
    Feature rootFeature = paraWorkspace.getRootFeature();
    Feature col = (Feature)rootFeature.getProperty( "BodenartCollectionMember" );
    List list = (List)col.getProperty( "BodenartMember" );
    Date calcDate = new Date();
    asciiBuffer.getBodartBuffer().append(
        "Bodenparameter NA-Modell, Datum " + ( calcDate.toString() ) + "\n" );
    asciiBuffer.getBodartBuffer().append(
        "BODART_ID ArtKap.  WP     FK     BFMAX     Kf   BF0\n" );
    asciiBuffer.getBodartBuffer().append(
        "                [mm/dm] [mm/dm] [mm/dm] [mm/d] [-]\n" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature bodenartFE = (Feature)iter.next();
//    TODO: nur die schreiben, die auch in Bodentyp verwendet werden.      
//      if( asciiBuffer.writeFeature( bodenartFE ) )
        writeFeature( asciiBuffer, paraWorkspace, bodenartFE );
    }

  }

  private void writeFeature( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace, Feature feature )
      throws Exception
  {
    asciiBuffer.getBodartBuffer().append( toAscci( feature, 6 ) + "\n" );
  }

}
