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
package org.kalypso.model.wspm.tuhh.core.profile.importer;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.model.wspm.tuhh.core.profile.importer.hw.HeightWidthCreator;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class ImportWProfShapeTest
{
  @Test
  public void importW80Shape( ) throws GmlSerializeException, IOException, CoreException
  {
    final NullProgressMonitor monitor = new NullProgressMonitor();

    final String fileBase = "P:\\nor0905940\\modell\\wprof\\Stary_Bren";
// final String fileBase = "P:\\bwg0715223\\modell\\WSPWin\\Modell_Ohrn\\work\\verl_Profile_Pkt";
// final String fileBase = "c:\\temp\\work\\verl_Profile_Pkt";
    final String sourceCrs = "EPSG:31467";
    final File tempDir = new File( FileUtilities.TMP_DIR, "WProf2Wspm" );
    tempDir.mkdirs();

    /* Load Shape */
    final GMLWorkspace w80shapeWorkspace = ShapeSerializer.deserialize( fileBase, sourceCrs, monitor );
    final FeatureList w80features = (FeatureList) w80shapeWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    System.out.println( String.format( "Read %d points", w80features.size() ) );

    /* Create Empty WSPM-Workspace */
// final GMLWorkspace targetWorkspace = FeatureFactory.createGMLWorkspace( TuhhWspmProject.QNAME, null, new
    // GmlSerializerFeatureProviderFactory() );
// final TuhhWspmProject project = new TuhhWspmProject( targetWorkspace.getRootFeature() );
    // final IWProfContentHandler creatorTUHH = new TuhhProfileWProfContentHandler( project, sourceCrs );
    final HeightWidthCreator creatorHW = new HeightWidthCreator( tempDir );
    final IWProfContentHandler[] creators = new IWProfContentHandler[] { /* creatorTUHH, */creatorHW };

    /* Data */
// final URL photoContext = new URL(
// "file:///C:\\users\\jung\\projekte\\bwg0715223\\HWGK_471_5_Hydraulik_work\\Vermessung\\" );
    final URL photoContext = null; // new URL( "file:///P:\\bwg0715223\\HWGK_471_5_Hydraulik_work\\Vermessung\\" );
// final URL photoContext = new URL(
    // "file:///P:\\bwg0715223\\gis\\Modell\\Querprofillagen\\Nachvermessungen\\Brettach_Mündung\\" );
    importW80Data( w80features, creators, photoContext );

    /* Write results */
    final File hwOutFile = new File( tempDir, "heightWidth.txt" );
    final File hwLogFile = new File( tempDir, "heightWidth.log" );
    creatorHW.writeToFile( hwOutFile, hwLogFile );

// final File targetFile = File.createTempFile( "modell_w80", ".gml" );
// GmlSerializer.serializeWorkspace( targetFile, targetWorkspace, "UTF-8" );
  }

  private void importW80Data( final FeatureList w80features, final IWProfContentHandler[] creators, final URL photoContext ) throws CoreException, MalformedURLException
  {
    final IWProfContentProvider importer = new PolnishBridgesWPRofContentProvider();
// final IWProfContentProvider importer = new BCEShapeWPRofContentProvider();

    for( final Object object : w80features )
    {
      final Feature feature = (Feature) object;

      final String riverId = importer.getRiverId( feature );
      final BigDecimal station = importer.getStation( feature );
      final GM_Point location = importer.getLocation( feature );
      final String comment = importer.getComment( feature );
      final String profileComment = importer.getProfileComment( feature );
      final String profileName = importer.getProfileName( feature );
      final String photoPath = importer.getPhotoPath( feature );
      final URL photoURL = photoPath == null ? null : new URL( photoContext, photoPath );
      final int ord = importer.getNumber( feature );
      final int partOrd = importer.getPartNumber( feature );
      final BigDecimal distance = importer.getDistance( feature );
      final double value = importer.getValue( feature );
      final String objectType = importer.getObjectType( feature );
      final int attributeType = importer.getType( feature );

      for( final IWProfContentHandler creator : creators )
        creator.newPoint( riverId, station, profileName, distance, location, value, comment, profileComment, photoURL, objectType, attributeType, ord, partOrd );
    }

    for( final IWProfContentHandler creator : creators )
      creator.finished();
  }
}
