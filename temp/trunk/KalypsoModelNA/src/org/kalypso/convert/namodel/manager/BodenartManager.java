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

package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author hübsch
 */
public class BodenartManager extends AbstractManager
{
  private final IFeatureType m_bodenartFT;

  public BodenartManager( GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );

    m_bodenartFT = parameterSchema.getFeatureType( "SoilLayer" );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  public String mapID( int id, IFeatureType ft )
  {
    throw new UnsupportedOperationException( " bodenartManager does not support int-ID mapping. (not necessary) " );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    // file
    // ) );
    Feature fe = null;
    // 3 Kommentarzeilen
    for( int i = 0; i <= 2; i++ )
    {
      String line;
      line = reader.readLine();
      if( line == null )
        return null;

      // TODO remove println
      System.out.println( reader.getLineNumber() + ": " + line );
    }
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return (Feature[]) result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 6
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );
    createProperties( propCollector, line, 6 );

    // generate id:
    String asciiStringId = propCollector.get( "name" );
    final Feature feature = getFeature( asciiStringId, m_bodenartFT );

    // continue reading

    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#getFeature(int,
   *      org.kalypsodeegree.model.feature.IFeatureType)
   */
  public Feature getFeature( int asciiID, IFeatureType ft )
  {
    throw new UnsupportedOperationException( " bodenartManager does not support int-ID mapping. (not necessary) " );
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace paraWorkspace ) throws Exception
  {
    Feature rootFeature = paraWorkspace.getRootFeature();
    List list = (List) rootFeature.getProperty( "soilLayerMember" );
    Date calcDate = new Date();
    asciiBuffer.getBodartBuffer().append( "Bodenparameter NA-Modell" + "\n" );
    asciiBuffer.getBodartBuffer().append( "BODART_ID ArtKap.  WP     FK     BFMAX     Kf   BF0\n" );
    asciiBuffer.getBodartBuffer().append( "                [mm/dm] [mm/dm] [mm/dm]  [mm/d] [-]\n" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature bodenartFE = (Feature) iter.next();
      // TODO: nur die schreiben, die auch in Bodentyp verwendet werden.
      writeFeature( asciiBuffer, bodenartFE );
    }

  }

  private void writeFeature( AsciiBuffer asciiBuffer, Feature feature ) throws Exception
  {
    // (name,*)_(typkap,*)_(typwp,*)_(typfk,*)_(typbfm,*)_(typkf,*)_(typbf0,*)
    asciiBuffer.getBodartBuffer().append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "name" ), "*" ) + " kap "
        + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typwp" ), "*" ) + " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typfk" ), "*" ) + " "
        + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typbfm" ), "*" ) + " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typkf" ), "*" ) + " "
        + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "typbf0" ), "*" ) + "\n" );
    // asciiBuffer.getBodartBuffer().append( toAscci( feature, 6 ) + "\n" );
  }
}
