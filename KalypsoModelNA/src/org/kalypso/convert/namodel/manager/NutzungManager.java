package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
/*
 * 
 * @author huebsch
 */
public class NutzungManager extends AbstractManager
{

  private final NAConfiguration m_conf;

  private final FeatureType m_nutzungFT;

  private final FeatureType m_nutzParameterFT;

  private static final String NutzParameterpropName = "NutzungParameterMember";

  public NutzungManager( GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    //    m_crs = crs;
    m_conf = conf;
    m_nutzungFT = parameterSchema.getFeatureType( "Nutzung" );
    m_nutzParameterFT = parameterSchema.getFeatureType( "NutzungParameter" );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, FeatureType ft )
  {
    return null;
  }

  /**
   * 
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    String nutzDatei = url.getPath().replaceAll( ".+/", "" );
    String nutzID = nutzDatei.replaceAll( "\\.nuz", "" );
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    // FileReader(
    // file
    // ) );
    Feature fe = null;
    while( ( fe = readNextFeature( reader, nutzID ) ) != null )
      result.add( fe );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader, String nutzID ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;
    // 9
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );
    createProperties( propCollector, line, 9 );
    //Kommentarzeilen
    line = reader.readLine();
    line = reader.readLine();

    //TODO: Filename == text (Nutzungsname)
    FeatureProperty nutzNameProp = FeatureFactory.createFeatureProperty( "text", nutzID );
    propCollector.put( "text", nutzNameProp );

    //  generate id:
    FeatureProperty prop = (FeatureProperty)propCollector.get( "text" );
    String asciiStringId = (String)prop.getValue();
    final Feature feature = getFeature( asciiStringId, m_nutzungFT );

    HashMap nutzungPropCollector = new HashMap();
    for( int i = 0; i < 12; i++ )
    {
      Feature nutzungParameterFeature = createFeature( m_nutzParameterFT );
      line = reader.readLine();
      System.out.println( "NutzParameter(" + i + "): " + line );
      createProperties( nutzungPropCollector, line, 10 );
      Collection collection = nutzungPropCollector.values();
      setParsedProperties( nutzungParameterFeature, collection );
      FeatureProperty nutzProp = FeatureFactory.createFeatureProperty( NutzParameterpropName, nutzungParameterFeature );
      feature.addProperty( nutzProp );
    }
    line = reader.readLine();

    // continue reading
    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    return feature;
  }

  public void writeFile( GMLWorkspace paraWorkspace ) throws Exception
  {
    Feature rootFeature = paraWorkspace.getRootFeature();
    Feature col = (Feature)rootFeature.getProperty( "NutzungCollectionMember" );
    List list = (List)col.getProperty( "NutzungMember" );
    Iterator iter = list.iterator();

    while( iter.hasNext() )
    {

      final Feature nutzungFE = (Feature)iter.next();
      //      if( asciiBuffer.writeFeature( nutzungFE ) )
      writeFeature( nutzungFE );
    }

  }

  private void writeFeature( Feature feature ) throws Exception
  {

    List nutzungList = (List)feature.getProperty( "NutzungParameterMember" );
    Iterator iter = nutzungList.iterator();
    String nutzName = FeatureHelper.getAsString( feature, "text" );
    File nutzungDir = new File( m_conf.getNutzungDir() + "\\" + nutzName + ".nuz" );
    FileWriter writer = new FileWriter( nutzungDir );
    writer.write( toAscci( feature, 9 ) + "\n" );
    writer.write( "Idealisierter Jahresgang\n" );
    writer.write( "xxdatum     F EVA    We    BIMAX\n" );

    while( iter.hasNext() )
    {
      Feature fe = (Feature)iter.next();
      writer.write( toAscci( fe, 10 ) + "\n" );
    }
    IOUtils.closeQuietly( writer );

  }

}
