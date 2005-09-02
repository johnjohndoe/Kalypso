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
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * Helper class to update mapping-gml for Katastropentest-Szenario
 * 
 * @author doemming
 */
public class KTestGMLUpdaterTest extends TestCase
{
  private ObjectFactory m_linkFac;

  private final Pattern p = Pattern.compile( "(.*)(HN)(\\..+\\..+\\.{3})([0-9]+)(.*)" );

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    KalypsoTest.init();
    m_linkFac = new ObjectFactory();
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
    // ombrometer
    String fileName = "ombrometer.gml";
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
    // Pegel Messung
    fileName = "PegelMessungMapping.gml";
    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
        "inObservationLink", "in1ObservationLink", "mappingMember" );
    // T Messung
    fileName = "ObsTMapping.gml";
    updateFeatureForKTest( getClass().getResource( resourceBase + fileName ), new File( outDir, fileName ),
        "inObservationLink", "in1ObservationLink", "mappingMember" );
    // TODO Ergebnisablage
  }

  /**
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
    Object real = feature.getProperty( realProp );
    if( real == null || !( real instanceof TimeseriesLink ) )
      return;
    final String href = ( (TimeseriesLink)real ).getHref();
    final String newHref = updateIDForKTest( href );
    if( newHref == null )
      return;
    final TimeseriesLink newLink = (TimeseriesLink)CloneUtilities.clone( real, m_linkFac );
    newLink.setHref( newHref );
    final FeatureProperty newProp = FeatureFactory.createFeatureProperty( testProp, newLink );
    feature.setProperty( newProp );
  }

  /**
   * ID für Katastrophentest-Szenario wird gebildet durch: <br>
   * Ersetzen von "HN" durch "TN" (Hauptnetz -> Testnetz) <br>
   * der Pegelkennziffer wird eine "9" vorrangestellt. <br>
   * Beispiel: "HN.1_ES.02PG...501060" -> "TN.1_ES.02PG...9501060"
   * 
   * @param href
   * @return new String
   */
  private String updateIDForKTest( String href )
  {
    if( href == null )
      return null;
    Matcher matcher = p.matcher( href );
    if( matcher.matches() )
    {
      String part1 = matcher.group( 1 );
      //      String part2_HN = matcher.group( 2 );
      String part3 = matcher.group( 3 );
      String part4_ID = matcher.group( 4 );
      String part5 = matcher.group( 5 );
      return part1 + "TN" + part3 + "9" + part4_ID + part5;
    }
    System.err.println( "can not generate K-TestID from >" + href + "<\n return >" + href + "<" );
    return href;
  }
}
