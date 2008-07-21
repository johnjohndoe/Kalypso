/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.tools.ktest;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.serialize.CloneUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Helper class to update mapping-gml for Katastropentest-Szenario
 * 
 * @author doemming
 */
public class KTestGMLUpdaterTest extends TestCase
{
  private static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private final Pattern p = Pattern.compile( "(.*)(HN)(\\..+\\..+\\.{3})([0-9]+)(.*)" );

  private static final int TRACK_MIDDLE = 1;

  private static final int TRACK_MIN = 2;

  private static final int TRACK_MAX = 3;

  public void testUpdateGML( ) throws Exception
  {
    // hier gegebenenfalls methoden auskomentieren
    weisseElster();
  }

  private void weisseElster( ) throws Exception
  {
    final String resourceBase = "resources/weisseElster/";

    // REMARK: please do never use system specific pathes for tests!
    final File outDir = FileUtilities.createNewTempDir( "update_k_test" );

    try
    {

      // Pegel Messung
      // und Ergebnisablage
      String fileName = "PegelMapping.gml";
      updatePegelMapping( new File( outDir, fileName ) );
      // ombrometer
      fileName = "ombrometer.gml";
      updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ), "NRepository", "NRepository1", "ombrometerMember" );
      // zufluss Messung
      fileName = "ZuflussMessungMapping.gml";
      updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ), "inObservationLink", "in1ObservationLink", "mappingMember" );
      // zufluss Vorhersage
      fileName = "ZuflussVorhersageMapping.gml";
      updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ), "inObservationLink", "in1ObservationLink", "mappingMember" );
      // T Messung
      fileName = "ObsTMapping.gml";
      updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ), "inObservationLink", "in1ObservationLink", "mappingMember" );
      // fileName = "PegelMessungMapping.gml";
      // updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
      // "inObservationLink", "in1ObservationLink", "mappingMember" );
    }
    finally
    {
      FileUtilities.deleteRecursive( outDir );
    }

  }

  /**
   * @param outFile
   * @throws Exception
   */
  private void updatePegelMapping( final File outFile ) throws Exception
  {
    // <!--
    // name PegelName
    // point ORT
    //
    // local1: Rechenfall/Pegel_gemessen.zml
    // local2: Rechenfall/Pegel_berechnet.zml
    // local3: Rechenfall/Pegel_berechnet_spurM.zml
    // local4: Rechenfall/Pegel_berechnet_spurU.zml
    // local5: Rechenfall/Pegel_berechnet_spurO.zml
    //
    // remote1: PSI-Pegel-Messung (ECHT)
    // remote2: PSI-Pegel-SpurM (ECHT)
    // remote3: PSI-Pegel-SpurU (ECHT)
    // remote4: PSI-Pegel-SpurO (ECHT)
    //
    // remote6: PSI-Pegel-Messung (TEST)
    // remote7: PSI-Pegel-SpurM (TEST)
    // remote8: PSI-Pegel-SpurU (TEST)
    // remote9: PSI-Pegel-SpurO (TEST)
    //
    // -->
    final URL modelURL = getClass().getResource( "resources/weisseElster/modell.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelURL, null );
    final GMLWorkspace mappingWorkspace = createEmptyMappingWorkspace();
    final IFeatureType mappingFT = mappingWorkspace.getFeatureType( "MappingObservation" );
    final Feature mapColFE = mappingWorkspace.getRootFeature();
    final Feature[] nodeFeatures = workspace.getFeatures( workspace.getFeatureType( "Node" ) );
    final String[][] pegelData = { new String[] { "Bad Elster", "kalypso-ocs:psicompact://HN.5_WE.02PG...576391" }, new String[] { "Adorf", "kalypso-ocs:psicompact://HN.5_WE.02PG...576400" },
        new String[] { "Oelsnitz", "kalypso-ocs:psicompact://HN.5_WE.02PG...576410" }, new String[] { "Strassberg", "kalypso-ocs:psicompact://HN.5_WE.02PG...576421" },
        new String[] { "Elsterberg", "kalypso-ocs:psicompact://HN.5_WE.02PG...576440" }, new String[] { "Rodewisch", "kalypso-ocs:psicompact://HN.5_WE.02PG...577211" },
        new String[] { "Mylau", "kalypso-ocs:psicompact://HN.5_WE.02PG...577220" }, new String[] { "Greiz", "kalypso-ocs:psicompact://HN.5_WE.02PG...576470" },
        new String[] { "Weida", "kalypso-ocs:psicompact://HN.5_WE.02PG...577320" }, new String[] { "Gera-Langenberg", "kalypso-ocs:psicompact://HN.5_WE.02PG...576520" },
        new String[] { "Zeitz", "kalypso-ocs:psicompact://HN.5_WE.02PG...576610" }, new String[] { "Kleindalzig", "kalypso-ocs:psicompact://HN.5_WE.02PG...576631" },
        new String[] { "Albrechtshain", "kalypso-ocs:psicompact://HN.5_WE.02PG...578090" }, new String[] { "Leipzig-Thekla", "kalypso-ocs:psicompact://HN.5_WE.02PG...578110" },
        new String[] { "Oberthau", "kalypso-ocs:psicompact://HN.5_WE.02PG...576900" }, new String[] { "Neukirchen", "kalypso-ocs:psicompact://HN.5_WE.02PG...577501" },
        new String[] { "Goessnitz", "kalypso-ocs:psicompact://HN.5_WE.02PG...577510" } };
    for( int i = 0; i < pegelData.length; i++ )
    {
      final String pegelName = pegelData[i][0];
      final String psiIDEcht = pegelData[i][1];
      System.out.print( i + " generate mapping for " + pegelName + " ..." );
      for( final Feature nodeFE : nodeFeatures )
      {
        if( pegelName.equals( FeatureHelper.getAsString( nodeFE, "name" ) ) )
        {
          final GM_Point point = (GM_Point) nodeFE.getProperty( "Ort" );
          final TimeseriesLinkType gemessen = (TimeseriesLinkType) nodeFE.getProperty( "pegelZR" );
          final TimeseriesLinkType berechnet = (TimeseriesLinkType) nodeFE.getProperty( "qberechnetZR" );

          final TimeseriesLinkType psiEcht = (TimeseriesLinkType) CloneUtilities.clone( berechnet, JC );
          psiEcht.setHref( psiIDEcht );
          final IRelationType linkPT = (IRelationType) mapColFE.getFeatureType().getProperty( "mappingMember" );
          final Feature mapFeature = mappingWorkspace.createFeature( mapColFE, linkPT, mappingFT );
          mapFeature.setProperty( "name", pegelName );
          mapFeature.setProperty( "point", point );
          mapFeature.setProperty( "local1", gemessen );
          mapFeature.setProperty( "local2", berechnet );
          mapFeature.setProperty( "local3", createTSLinkForTrackLocal( berechnet, TRACK_MIDDLE ) );
          mapFeature.setProperty( "local4", createTSLinkForTrackLocal( berechnet, TRACK_MIN ) );
          mapFeature.setProperty( "local5", createTSLinkForTrackLocal( berechnet, TRACK_MAX ) );

          mapFeature.setProperty( "remote1", psiEcht );
          mapFeature.setProperty( "remote2", createTSLinkForTrackRemote( psiEcht, TRACK_MIDDLE ) );
          mapFeature.setProperty( "remote3", createTSLinkForTrackRemote( psiEcht, TRACK_MIN ) );
          mapFeature.setProperty( "remote4", createTSLinkForTrackRemote( psiEcht, TRACK_MAX ) );

          mapFeature.setProperty( "remote6", createTSLinkForKTest( psiEcht ) );
          mapFeature.setProperty( "remote7", createTSLinkForTrackRemote( createTSLinkForKTest( psiEcht ), TRACK_MIDDLE ) );
          mapFeature.setProperty( "remote8", createTSLinkForTrackRemote( createTSLinkForKTest( psiEcht ), TRACK_MIN ) );
          mapFeature.setProperty( "remote9", createTSLinkForTrackRemote( createTSLinkForKTest( psiEcht ), TRACK_MAX ) );
          mappingWorkspace.addFeatureAsComposition( mapColFE, linkPT, 0, mapFeature );
          System.out.println( "... done" );
          break;
        }
      }
      System.out.println();
    }
    FileWriter writer = null;
    try
    {
      writer = new FileWriter( outFile );
      GmlSerializer.serializeWorkspace( writer, mappingWorkspace, "UTF-8" );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * @param linkLocal
   * @param track
   * @return new href
   * @throws JAXBException
   */
  private TimeseriesLinkType createTSLinkForTrackLocal( final TimeseriesLinkType linkLocal, final int track ) throws JAXBException
  {
    // letzen teil weg
    final String prefix;
    switch( track )
    {
      case TRACK_MIDDLE:
        prefix = "SpurM_";
        break;
      case TRACK_MIN:
        prefix = "SpurU_";
        break;
      case TRACK_MAX:
        prefix = "SpurO_";
        break;
      default:
        throw new UnsupportedOperationException( "unsupported track" );
    }
    final String href = linkLocal.getHref();
    final int split = href.lastIndexOf( "/" );
    final String part1 = href.substring( 0, split );
    final String part3 = href.substring( split + 1 );
    final String newHref = part1 + "/Ablage/" + prefix + part3;
    // href.replaceAll( "/.+?", href ) + "/Ablage/" + prefix + href.replaceAll( ".+/", "" );
    final TimeseriesLinkType newLink = (TimeseriesLinkType) CloneUtilities.clone( linkLocal, JC );
    newLink.setHref( newHref );
    return newLink;
  }

  private GMLWorkspace createEmptyMappingWorkspace( ) throws Exception
  {
    return GmlSerializer.createGMLWorkspace( getClass().getResource( "resources/EmptyMapping.gml" ), null );
  }

  /**
   * update the ktestprop from realprop
   * 
   * @param featurePath
   * @throws Exception
   * @throws Exception
   */
  private void updateFeatureForKTest( final URL input, final File output, final String realProp, final String kTestProp, final String featurePath ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( input, null );
    final Object featureFromPath = workspace.getFeatureFromPath( featurePath );
    if( featureFromPath instanceof Feature )
      updateFeature( (Feature) featureFromPath, realProp, kTestProp );
    else if( featureFromPath instanceof FeatureList )
    {
      final Iterator iterator = ((FeatureList) featureFromPath).iterator();
      while( iterator.hasNext() )
        updateFeature( (Feature) iterator.next(), realProp, kTestProp );
    }
    output.getParentFile().mkdirs();
    Writer writer = null;
    try
    {
      writer = new FileWriter( output );
      GmlSerializer.serializeWorkspace( writer, workspace, "UTF-8" );
      System.out.println( "wrote file " + output.getAbsolutePath() );
    }
    catch( final Exception e )
    {
      throw e;
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }

  }

  /**
   * @param feature
   * @param testProp
   * @param realProp
   * @throws JAXBException
   */
  private void updateFeature( final Feature feature, final String realProp, final String testProp ) throws JAXBException
  {
    final Object real = feature.getProperty( realProp );
    if( real == null || !(real instanceof TimeseriesLinkType) )
      return;
    final TimeseriesLinkType newLink = createTSLinkForKTest( (TimeseriesLinkType) real );
    feature.setProperty( testProp, newLink );
  }

  private TimeseriesLinkType createTSLinkForKTest( final TimeseriesLinkType linkEcht ) throws JAXBException
  {
    final String href = linkEcht.getHref();
    final String newHref = ceateIDForKTest( href );
    final TimeseriesLinkType linkKTEST = (TimeseriesLinkType) CloneUtilities.clone( linkEcht, JC );
    linkKTEST.setHref( newHref );
    return linkKTEST;
  }

  private TimeseriesLinkType createTSLinkForTrackRemote( final TimeseriesLinkType link, final int track ) throws JAXBException
  {
    final String href = link.getHref();
    final String newHref = ceateIDForTrackRemote( href, track );
    final TimeseriesLinkType newLink = (TimeseriesLinkType) CloneUtilities.clone( link, JC );
    newLink.setHref( newHref );
    return newLink;
  }

  /**
   * ID f�r Prognoseablage wird durch Anh�ngen von <br>
   * ".P1_MW" (unterer Prognosewert), <br>
   * ".P2_MW" (oberer Prognosewert), <br>
   * ".P3_MW" (berechnter Prognosewert) <br>
   * gebildet
   * 
   * @param href
   * @return new id
   */
  private String ceateIDForTrackRemote( final String href, final int track )
  {
    switch( track )
    {
      case TRACK_MIDDLE:
        return href + ".P3_MW";
      case TRACK_MIN:
        return href + ".P1_MW";
      case TRACK_MAX:
        return href + ".P2_MW";
    }
    return null;
  }

  /**
   * ID f�r Katastrophentest-Szenario wird gebildet durch: <br>
   * Ersetzen von "HN" durch "TN" (Hauptnetz -> Testnetz) <br>
   * der Pegelkennziffer wird eine "9" vorrangestellt. <br>
   * Beispiel: "HN.1_ES.02PG...501060" -> "TN.1_ES.02PG...9501060"
   * 
   * @param hrefECHT
   * @return new String
   */
  private String ceateIDForKTest( final String hrefECHT )
  {
    if( hrefECHT == null )
      return null;
    final Matcher matcher = p.matcher( hrefECHT );
    if( matcher.matches() )
    {
      final String part1 = matcher.group( 1 );
      // String part2_HN = matcher.group( 2 );
      final String part3 = matcher.group( 3 );
      final String part4_ID = matcher.group( 4 );
      final String part5 = matcher.group( 5 );
      return part1 + "TN" + part3 + "9" + part4_ID + part5;
    }
    System.err.println( "can not generate K-TestID from >" + hrefECHT + "<\n return >" + hrefECHT + "<" );
    return hrefECHT;
  }
}
