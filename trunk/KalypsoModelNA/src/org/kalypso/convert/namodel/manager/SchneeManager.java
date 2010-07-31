/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class SchneeManager extends AbstractManager
{

  private final IFeatureType m_snowFT;

  /*
   * @author huebsch
   */
  public SchneeManager( final GMLSchema parameterSchema, final NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_snowFT = parameterSchema.getFeatureType( NaModelConstants.PARA_SNOW_NAME );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( final int id, final IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( final URL url ) throws Exception
  {
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    // FileReader(
    // file
    // ) );
    Feature fe = null;
    // Kommentarzeilen
    for( int i = 0; i <= 2; i++ )
    {
      String line;
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$
    }
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( final LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 6
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$
    createProperties( propCollector, line, 13 );

    // generate id:
    // final FeatureProperty prop = (FeatureProperty)propCollector.get( "name" );
    final String asciiStringId = propCollector.get( "name" ); //$NON-NLS-1$
    final Feature feature = getFeature( asciiStringId, m_snowFT );

    // continue reading

    // Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace paraWorkspace ) throws Exception
  {
    final Feature rootFeature = paraWorkspace.getRootFeature();
    final List< ? > list = (List< ? >) rootFeature.getProperty( NaModelConstants.PARA_PROP_SNOW_MEMBER );
    asciiBuffer.getSnowBuffer().append( Messages.getString( "org.kalypso.convert.namodel.manager.SchneeManager.0" ) ); //$NON-NLS-1$
    asciiBuffer.getSnowBuffer().append( "/                     wwo wwmax snotem snorad h0\n" ); //$NON-NLS-1$
    asciiBuffer.getSnowBuffer().append( "/                      *    *     *      *    *\n" ); //$NON-NLS-1$
    final Iterator< ? > iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature snowFE = (Feature) iter.next();
      // TODO: nur die schreiben, die auch in Gebietsdatei vorkommen
      writeFeature( asciiBuffer, snowFE );
    }

  }

  private void writeFeature( final AsciiBuffer asciiBuffer, final Feature feature ) throws Exception
  {
    asciiBuffer.getSnowBuffer().append( toAscci( feature, 13 ) + "\n" ); //$NON-NLS-1$
  }

}
