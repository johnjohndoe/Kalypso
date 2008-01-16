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
package org.kalypso.ogc.gml.serialize.test;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * @author Gernot Belger
 */
public class SpatialIndexTest extends TestCase
{
  private final List<File> m_filesToDelete = new ArrayList<File>();

  /**
   * @see junit.framework.TestCase#tearDown()
   */
  @Override
  protected void tearDown( ) throws Exception
  {
    for( final File file : m_filesToDelete )
      FileUtils.forceDelete( file );

    super.tearDown();
  }

  public void testBigShape( ) throws Exception
  {
    doTheTest( "resources/bigShape.zip", "mod", ShapeSerializer.PROPERTY_FEATURE_MEMBER );
  }

  public void testBigGml( ) throws Exception
  {
    doTheTest( "resources/bigGml.zip", "nodeResult.gml", new QName( "http://www.tu-harburg.de/wb/kalypso/schemata/1d2dResults", "nodeResultMember" ) );
  }

  private void doTheTest( final String zipResourcePath, final String filename, final QName propQName ) throws Exception
  {
    final TimeLogger logger = new TimeLogger( "Start spatial index test" );
    final GMLWorkspace workspace = loadWorkspace( zipResourcePath, filename );
    logger.takeInterimTime();
    logger.printCurrentInterim( "File loaded in: " );

    final SplitSort sort = new SplitSort( null, null );

    final FeatureList featureList = (FeatureList) workspace.getRootFeature().getProperty( propQName );
    for( final Object object : featureList )
      sort.add( object );

    logger.takeInterimTime();
    logger.printCurrentInterim( "Index built in: " );

    final GM_Envelope boundingBox = featureList.getBoundingBox();

    sort.query( boundingBox, null );
    logger.takeInterimTime();
    logger.printCurrentInterim( "Index queried in: " );

    sort.query( boundingBox, null );
    logger.takeInterimTime();
    logger.printCurrentInterim( "Index queried again in: " );

    sort.invalidate( featureList.first() );
    logger.takeInterimTime();
    logger.printCurrentInterim( "Index invalidated in: " );

    sort.query( boundingBox, null );
    logger.takeInterimTime();
    logger.printCurrentInterim( "Index queried again in: " );

    logger.printCurrentTotal( "Total: " );
  }

  private GMLWorkspace loadWorkspace( final String relativeResourcePath, final String filename ) throws Exception
  {
    final URL resource = getClass().getResource( relativeResourcePath );

    final File unzipDir = FileUtilities.createNewTempDir( "unzip" );

    ZipUtilities.unzip( resource, unzipDir );

    final File fileBase = new File( unzipDir, filename );

    if( filename.toLowerCase().endsWith( ".gml" ) )
      return GmlSerializer.createGMLWorkspace( fileBase.toURL(), null );

    return ShapeSerializer.deserialize( fileBase.getAbsolutePath(), KalypsoCorePlugin.getDefault().getCoordinatesSystem() );
  }
}
