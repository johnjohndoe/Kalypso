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

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.KalypsoTest;
import org.kalypso.ogc.gml.serialize.CloneUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Helper class to update mapping-gml for Katastropentest-Szenario
 * 
 * @author doemming
 */
public class KTestGMLUpdaterTest extends TestCase
{
  private ObjectFactory m_linkFac;

  private final Pattern p = Pattern.compile( "(.*)(HN)(\\..+\\..+\\.{3})([0-9]+)(.*)" );

  private static final int TRACK_MIDDLE = 1;

  private static final int TRACK_MIN = 2;

  private static final int TRACK_MAX = 3;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    try
    {
      KalypsoTest.init();
      m_linkFac = new ObjectFactory();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public void testUpdateGML() throws Exception
  {
    // hier gegebenenfalls methoden auskomentieren
    weisseElster();
  }

  private void weisseElster() throws Exception
  {
    final String resourceBase = "resources/weisseElster/";
    final File outDir = new File( "C:\\TMP\\update_k_test" );
    // Pegel Messung
    // und Ergebnisablage
    String fileName = "PegelMapping.gml";
    updatePegelMapping( new File( outDir, fileName ) );
    // ombrometer
    fileName = "ombrometer.gml";
    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
        "NRepository", "NRepository1", "ombrometerMember" );
    // zufluss Messung
    fileName = "ZuflussMessungMapping.gml";
    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
        "inObservationLink", "in1ObservationLink", "mappingMember" );
    // zufluss Vorhersage
    fileName = "ZuflussVorhersageMapping.gml";
    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
        "inObservationLink", "in1ObservationLink", "mappingMember" );
    // T Messung
    fileName = "ObsTMapping.gml";
    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
        "inObservationLink", "in1ObservationLink", "mappingMember" );
    //    fileName = "PegelMessungMapping.gml";
    //    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
    //        "inObservationLink", "in1ObservationLink", "mappingMember" );

  }

  /**
   * @param outFile
   * @throws Exception
   *  
   */
  private void updatePegelMapping( File outFile ) throws Exception
  {
    //    <!--
    //    name PegelName
    //    point ORT
    //
    //    local1: Rechenfall/Pegel_gemessen.zml
    //    local2: Rechenfall/Pegel_berechnet.zml
    //    local3: Rechenfall/Pegel_berechnet_spurM.zml
    //    local4: Rechenfall/Pegel_berechnet_spurU.zml
    //    local5: Rechenfall/Pegel_berechnet_spurO.zml
    //
    //    remote1: PSI-Pegel-Messung (ECHT)
    //    remote2: PSI-Pegel-SpurM (ECHT)
    //    remote3: PSI-Pegel-SpurU (ECHT)
    //    remote4: PSI-Pegel-SpurO (ECHT)
    //
    //    remote6: PSI-Pegel-Messung (TEST)
    //    remote7: PSI-Pegel-SpurM (TEST)
    //    remote8: PSI-Pegel-SpurU (TEST)
    //    remote9: PSI-Pegel-SpurO (TEST)
    //
    //     -->
    final URL modelURL = getClass().getResource( "resources/weisseElster/modell.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelURL );
    final GMLWorkspace mappingWorkspace = createEmptyMappingWorkspace();
    final FeatureType mappingFT = mappingWorkspace.getFeatureType( "MappingObservation" );
    final Feature mapColFE = mappingWorkspace.getRootFeature();
    final Feature[] nodeFeatures = workspace.getFeatures( workspace.getFeatureType( "Node" ) );
    final String[][] pegelData =
    {
        new String[]
        {
            "Bad Elster",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576391" },
        new String[]
        {
            "Adorf",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576400" },
        new String[]
        {
            "Oelsnitz",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576410" },
        new String[]
        {
            "Strassberg",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576421" },
        new String[]
        {
            "Elsterberg",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576440" },
        new String[]
        {
            "Rodewisch",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...577211" },
        new String[]
        {
            "Mylau",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...577220" },
        new String[]
        {
            "Greiz",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576470" },
        new String[]
        {
            "Weida",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...577320" },
        new String[]
        {
            "Gera-Langenberg",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576520" },
        new String[]
        {
            "Zeitz",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576610" },
        new String[]
        {
            "Kleindalzig",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576631" },
        new String[]
        {
            "Albrechtshain",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...578090" },
        new String[]
        {
            "Leipzig-Thekla",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...578110" },
        new String[]
        {
            "Oberthau",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...576900" },
        new String[]
        {
            "Neukirchen",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...577501" },
        new String[]
        {
            "Goessnitz",
            "kalypso-ocs:psicompact://HN.5_WE.02PG...577510" } };
    for( int i = 0; i < pegelData.length; i++ )
    {
      final String pegelName = pegelData[i][0];
      final String psiIDEcht = pegelData[i][1];
      System.out.print( i + " generate mapping for " + pegelName + " ..." );
      for( int j = 0; j < nodeFeatures.length; j++ )
      {
        final Feature nodeFE = nodeFeatures[j];
        if( pegelName.equals( FeatureHelper.getAsString( nodeFE, "name" ) ) )
        {
          final GM_Point point = (GM_Point)nodeFE.getProperty( "Ort" );
          final TimeseriesLink gemessen = (TimeseriesLink)nodeFE.getProperty( "pegelZR" );
          final TimeseriesLink berechnet = (TimeseriesLink)nodeFE.getProperty( "qberechnetZR" );

          final TimeseriesLink psiEcht = (TimeseriesLink)CloneUtilities.clone( berechnet, m_linkFac );
          psiEcht.setHref( psiIDEcht );
          final Feature mapFeature = mappingWorkspace.createFeature( mappingFT );
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

          mapFeature.setProperty( "remote6",  createTSLinkForKTest( psiEcht ) );
          mapFeature
              .setProperty( "remote7", createTSLinkForTrackRemote( createTSLinkForKTest( psiEcht ), TRACK_MIDDLE ) );
          mapFeature.setProperty( "remote8", createTSLinkForTrackRemote( createTSLinkForKTest( psiEcht ), TRACK_MIN ) );
          mapFeature.setProperty( "remote9", createTSLinkForTrackRemote( createTSLinkForKTest( psiEcht ), TRACK_MAX ) );
          mappingWorkspace.addFeatureAsComposition( mapColFE, "mappingMember", 0, mapFeature );
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
  private TimeseriesLink createTSLinkForTrackLocal( final TimeseriesLink linkLocal, final int track )
      throws JAXBException
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
    int split = href.lastIndexOf( "/" );
    String part1 = href.substring( 0, split );
    String part3 = href.substring( split +1);
    final String newHref = part1 + "/Ablage/" + prefix + part3;
    //      href.replaceAll( "/.+?", href ) + "/Ablage/" + prefix + href.replaceAll( ".+/", "" );
    final TimeseriesLink newLink = (TimeseriesLink)CloneUtilities.clone( linkLocal, m_linkFac );
    newLink.setHref( newHref );
    return newLink;
  }

  private GMLWorkspace createEmptyMappingWorkspace() throws Exception
  {
    return GmlSerializer.createGMLWorkspace( getClass().getResource( "resources/EmptyMapping.gml" ) );
  }

  /**
   * update the ktestprop from realprop
   * 
   * @param featurePath
   * @throws Exception
   * @throws Exception
   *  
   */
  private void updateFeatureForKTest( final URL input, final File output, final String realProp,
      final String kTestProp, final String featurePath ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( input );
    final Object featureFromPath = workspace.getFeatureFromPath( featurePath );
    if( featureFromPath instanceof Feature )
      updateFeature( (Feature)featureFromPath, realProp, kTestProp );
    else if( featureFromPath instanceof FeatureList )
    {
      Iterator iterator = ( (FeatureList)featureFromPath ).iterator();
      while( iterator.hasNext() )
        updateFeature( (Feature)iterator.next(), realProp, kTestProp );
    }
    output.getParentFile().mkdirs();
    Writer writer = null;
    try
    {
      writer = new FileWriter( output );
      GmlSerializer.serializeWorkspace( writer, workspace, "UTF-8" );
      System.out.println( "wrote file " + output.getAbsolutePath() );
    }
    catch( Exception e )
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
  private void updateFeature( final Feature feature, final String realProp, final String testProp )
      throws JAXBException
  {
    final Object real = feature.getProperty( realProp );
    if( real == null || !( real instanceof TimeseriesLink ) )
      return;
    final TimeseriesLink newLink = createTSLinkForKTest( (TimeseriesLink)real );
    final FeatureProperty newProp = FeatureFactory.createFeatureProperty( testProp, newLink );
    feature.setProperty( newProp );
  }

  private TimeseriesLink createTSLinkForKTest( final TimeseriesLink linkEcht ) throws JAXBException
  {
    final String href = linkEcht.getHref();
    final String newHref = ceateIDForKTest( href );
    final TimeseriesLink linkKTEST = (TimeseriesLink)CloneUtilities.clone( linkEcht, m_linkFac );
    linkKTEST.setHref( newHref );
    return linkKTEST;
  }

  private TimeseriesLink createTSLinkForTrackRemote( final TimeseriesLink link, int track ) throws JAXBException
  {
    final String href = link.getHref();
    final String newHref = ceateIDForTrackRemote( href, track );
    final TimeseriesLink newLink = (TimeseriesLink)CloneUtilities.clone( link, m_linkFac );
    newLink.setHref( newHref );
    return newLink;
  }

  /**
   * ID für Prognoseablage wird durch Anhängen von <br>
   * ".P1_MW" (unterer Prognosewert), <br>
   * ".P2_MW" (oberer Prognosewert), <br>
   * ".P3_MW" (berechnter Prognosewert) <br>
   * gebildet
   * 
   * @param href
   * @return new id
   */
  private String ceateIDForTrackRemote( String href, int track )
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
   * ID für Katastrophentest-Szenario wird gebildet durch: <br>
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
    Matcher matcher = p.matcher( hrefECHT );
    if( matcher.matches() )
    {
      String part1 = matcher.group( 1 );
      //      String part2_HN = matcher.group( 2 );
      String part3 = matcher.group( 3 );
      String part4_ID = matcher.group( 4 );
      String part5 = matcher.group( 5 );
      return part1 + "TN" + part3 + "9" + part4_ID + part5;
    }
    System.err.println( "can not generate K-TestID from >" + hrefECHT + "<\n return >" + hrefECHT + "<" );
    return hrefECHT;
  }
}
