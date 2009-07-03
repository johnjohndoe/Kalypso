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

package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author huebsch
 */
public class BodentypManager extends AbstractManager
{
  private final NAConfiguration m_conf;

  private final FeatureType m_bodentypFT;

  private final FeatureType m_bodenartFT;

  private static final String BodArtParameterPropName = "BodArtParameterMember";

  public BodentypManager( GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_conf = conf;
    m_bodentypFT = parameterSchema.getFeatureType( "Bodentyp" );
    m_bodenartFT = parameterSchema.getFeatureType( "BodArtParameter" );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    // FileReader(
    // file
    // ) );
    Feature fe = null;
    //  Kommentarzeilen
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
    // 1
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );
    createProperties( propCollector, line, 1 );

    //  generate id:
    FeatureProperty prop = (FeatureProperty)propCollector.get( "text6" );
    String asciiStringId = (String)prop.getValue();
    final Feature feature = getFeature( asciiStringId, m_bodentypFT );

    FeatureProperty ianzProp = (FeatureProperty)propCollector.get( "ianz" );
    int ianz = Integer.parseInt( (String)ianzProp.getValue() );
    HashMap bodArtPropCollector = new HashMap();
    // BodArtParameterMember
    for( int i = 0; i < ianz; i++ )
    {
      Feature bodArtParameterFeature = createFeature( m_bodenartFT );
      line = reader.readLine();
      System.out.println( "bodart(" + i + "): " + line );
      createProperties( bodArtPropCollector, line, 2 );
      // BodArtLink
      final FeatureProperty BodArtNameProp = (FeatureProperty)bodArtPropCollector.get( "char3" );
      String asciiBodArtId = (String)( BodArtNameProp.getValue() );
      final Feature BodArtFE = getFeature( asciiBodArtId, m_conf.getBodartFT() );
      final FeatureProperty BodArtLink = FeatureFactory.createFeatureProperty( "BodArtLink", BodArtFE.getId() );
      bodArtParameterFeature.setProperty( BodArtLink );

      Collection collection = bodArtPropCollector.values();
      setParsedProperties( bodArtParameterFeature, collection );
      FeatureProperty bodArtProp = FeatureFactory.createFeatureProperty( BodArtParameterPropName,
          bodArtParameterFeature );
      feature.addProperty( bodArtProp );
    }

    // continue reading
    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace ) throws Exception
  {
    Feature rootFeature = paraWorkspace.getRootFeature();
    Feature col = (Feature)rootFeature.getProperty( "BodentypCollectionMember" );
    List list = (List)col.getProperty( "BodentypMember" );
    asciiBuffer.getBodtypBuffer().append( "/Bodentypen:\n" );
    asciiBuffer.getBodtypBuffer().append( "/\n" );
    asciiBuffer.getBodtypBuffer().append( "/Typ       Tiefe[dm]\n" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {

      final Feature bodentypFE = (Feature)iter.next();
      //TODO: nur die schreiben, die auch in Hydrotopdatei vorkommen
      //      if( asciiBuffer.writeFeature( bodentypFE ) )
      writeFeature( asciiBuffer, paraWorkspace, bodentypFE );
    }

  }

  private void writeFeature( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace, Feature feature ) throws Exception
  {
    // 1
    List bodartList = (List)feature.getProperty( BodArtParameterPropName );
    asciiBuffer.getBodtypBuffer().append(
        FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "text6" ), "a10" )
            + FortranFormatHelper.printf( Integer.toString( bodartList.size() ), "i4" ) + "\n" );
    // 2
    Iterator iter = bodartList.iterator();
    while( iter.hasNext() )
    {
      Feature fe = (Feature)iter.next();
      Feature BodArtLink = paraWorkspace.resolveLink( fe, "BodArtLink" );
      if( BodArtLink != null )
      {
        asciiBuffer.getBodtypBuffer().append(
            FortranFormatHelper.printf( FeatureHelper.getAsString( BodArtLink, "typchar" ), "a8" )
                + FortranFormatHelper.printf( FeatureHelper.getAsString( fe, "xtief" ), "*" ) + " "
                + FortranFormatHelper.printf( FeatureHelper.getAsString( fe, "xret" ), "*" ) + "\n" );
      }
      else
      {
        // TODO use logger
        System.out.println( "Fehler in der Bodentypdatei zu Bodentyp " + feature.getId() + " (Link zu Bodenart "
            + fe.getProperty( "BodArtLink" ) + " nicht vorhanden)" );
      }
    }

  }

}
