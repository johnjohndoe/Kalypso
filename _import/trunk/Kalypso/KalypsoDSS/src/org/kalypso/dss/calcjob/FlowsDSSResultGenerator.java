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
package org.kalypso.dss.calcjob;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.tokenreplace.ITokenReplacer;
import org.kalypso.commons.tokenreplace.TokenReplacerEngine;
import org.kalypso.convert.namodel.DefaultPathGenerator;
import org.kalypso.dss.KalypsoDSSPlugin;
import org.kalypso.dss.utils.MeasuresConstants;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.util.ZMLUtilities;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class FlowsDSSResultGenerator
{

  final static ITokenReplacer titleReplacer = new ITokenReplacer()
  {

    public String getToken( )
    {
      return "title";
    }

    public String replaceToken( Object value, String argument )
    {
      final Combination combination = (Combination) value;
      final String hqEventId = combination.getHQEventId();
      final Feature nodeFE = combination.getNodeFeature();
      final String nodeNme = (String) nodeFE.getProperty( "name" );
      return hqEventId + " - " + nodeNme;
    }
  };

  private static TokenReplacerEngine ODT_TokenReplaceEngine = new TokenReplacerEngine( new ITokenReplacer[] { titleReplacer } );

  /**
   * @param noMeasures
   *          true, if calculation is based only on planing without measures
   */
  public static void generateDssResultFor( final File dssResultDir, final File rrmResultDir, final ISimulationDataProvider inputProvider, final String hqEventId, final Feature resultNode, final boolean doMeasures ) throws MalformedURLException
  {
    final String nodeName = (String) resultNode.getProperty( "name" );
    final String resultTitle = hqEventId + " - " + nodeName;

    // find sources for new result locations
    final URL srcStatusQuoFile = getStatusQuo( hqEventId, resultNode );
    final String path = DefaultPathGenerator.generateResultPathFor( resultNode, "name", "qgs", null );
    final File newNodeResultFile = new File( rrmResultDir, path );

    // define new result locations
    // final File dssResultContainer = getContainerPath( dssResultDir, resultNode, hqEventId );

    final String containerPath = getContainerPath( resultNode );
    final File newStatusQuoFile = new File( dssResultDir, containerPath + "/statusQuo.zml" );
    final File newPlanFile = new File( dssResultDir, containerPath + "/mitPlanung.zml" );
    final File newPlanAndMeasuerFile = new File( dssResultDir, containerPath + "/mitPlanungUndMassnahmen.zml" );

    final File newAnalysisOdtFile = new File( dssResultDir, containerPath + "/analyse.odt" );
    final File newAnalysisHTMLFile = new File( dssResultDir, containerPath + "/analyse.html" );

    URL srcPlanURL = null;
    final URL srcPlanAndMeasuerURL;

    if( doMeasures )
    {
      srcPlanAndMeasuerURL = newNodeResultFile.toURL();
      // retrieve file from inputdata
      try
      {
        final URL lastResultContainerURL = (URL) inputProvider.getInputForID( MeasuresConstants.IN_LastResults );
        srcPlanURL = new URL( lastResultContainerURL,"Ergebnisse/" +hqEventId + "/" + containerPath + "/mitPlanung.zml" );
      }
      catch( Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    else
    {
      srcPlanURL = newNodeResultFile.toURL();
      // srcPlanAndMeasure is obsolete
      srcPlanAndMeasuerURL = null;
    }

    // put them into the dss results
    OutputStream outputStream = null;
    // handle statusQuo
    try
    {
      newStatusQuoFile.getParentFile().mkdirs();
      outputStream = new FileOutputStream( newStatusQuoFile );
      IOUtils.copy( srcStatusQuoFile.openStream(), outputStream );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( outputStream );
    }

    // handle planing
    try
    {
      newPlanFile.getParentFile().mkdirs();
      outputStream = new FileOutputStream( newPlanFile );
      IOUtils.copy( srcPlanURL.openStream(), outputStream );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( outputStream );
    }

    // handle planing and measure
    try
    {
      if( doMeasures )
      {
        newPlanAndMeasuerFile.getParentFile().mkdirs();
        outputStream = new FileOutputStream( newPlanAndMeasuerFile );
        IOUtils.copy( srcPlanAndMeasuerURL.openStream(), outputStream );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( outputStream );
    }

    // generate analysis odt
    final URL resource = FlowsDSSResultGenerator.class.getResource( "resources/AnalyseTemplate.odt" );
    String odtAsString = null;
    InputStream input = null;
    final StringWriter writer = new StringWriter();
    try
    {
      input = resource.openStream();
      IOUtils.copy( input, writer );
      odtAsString = writer.toString();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( input );
    }

    // StreamUtilities.streamCopy(resource.openStream(), writer);
    final Combination value = new Combination( resultNode, hqEventId );
    odtAsString = ODT_TokenReplaceEngine.replaceTokens( value, odtAsString );

    OutputStream odtOutputStream = null;
    try
    {
      odtOutputStream = new FileOutputStream( newAnalysisOdtFile );
      IOUtils.write( odtAsString, odtOutputStream );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( odtOutputStream );
    }

    // analyse files
    boolean gotStatusQuo = false;
    boolean gotPlaning = false;
    boolean gotPlaningAndMeasure = false;

    double maxStatusQuoQ = 0;
    double maxPlaningQ = 0;
    double maxPlaningAndMeasureQ = 0;
    try
    {
      maxStatusQuoQ = ZMLUtilities.getMax( srcStatusQuoFile, TimeserieConstants.TYPE_RUNOFF, null );
      gotStatusQuo = true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    try
    {
      maxPlaningQ = ZMLUtilities.getMax( srcPlanURL, TimeserieConstants.TYPE_RUNOFF, null );
      gotPlaning = true;
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    try
    {
      if( doMeasures )
      {
        maxPlaningAndMeasureQ = ZMLUtilities.getMax( srcPlanAndMeasuerURL, TimeserieConstants.TYPE_RUNOFF, null );
        gotPlaningAndMeasure = true;
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    if( gotStatusQuo && gotPlaning && gotPlaningAndMeasure )
    {
      final String pageHTML = generateHTML( maxStatusQuoQ, maxPlaningQ, maxPlaningAndMeasureQ, hqEventId );
      OutputStream htmlOutputStream = null;
      try
      {
        htmlOutputStream = new FileOutputStream( newAnalysisHTMLFile );
        IOUtils.write( pageHTML, htmlOutputStream );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  private static String generateHTML( double maxStatusQuoQ, double maxPlaningQ, double maxPlaningAndMeasureQ, String hqEventId )
  {
    double overLoadQ = maxPlaningQ - maxStatusQuoQ;
    double reducedQ = maxPlaningQ - maxPlaningAndMeasureQ;
    final int reducedPercent;
    if( overLoadQ == reducedQ )
      reducedPercent = 100;
    else
      reducedPercent = (int) (100d / (overLoadQ * reducedQ));
    final String barHTML = generateBarHTML( reducedPercent );
    final String tableHTML = generateTableHTML( maxStatusQuoQ, maxPlaningQ, maxPlaningAndMeasureQ, hqEventId );
    final String legendHTML = generateLegendHTMLPart();
    return "<html><body bgcolor=\"#FFFFCC\">"//
        + barHTML + //
        "<br>"//  
        + tableHTML//
        + legendHTML//
        + "<br>"//
        + "</body></html>";
  }

  private final static String GREEN = "#339933";

  private final static String RED = "#CC3333";

  private static String generateBarHTML( final int reducedPercent )
  {
    return " "// 
        + "<table border=\"0\" width=\"100%\" height=\"30\">"//
        + "   <colgroup>"//
        + "     <col width=\"" + reducedPercent + "%\">"//
        + "     <col width=\"" + (100 - reducedPercent) + "%\">"//
        + "   </colgroup>"//
        + "   <tr>"//
        + "     <td bgcolor=\"" + GREEN + "\"></td>"//
        + "     <td bgcolor=\"" + RED + "\"></td>"//
        + "   </tr>"//
        + "</table>";
  }

  private static String generateLegendHTMLPart( )
  {
    return "<!-- legend -->"//
        + "<table border=\"2\" align=\"right\">"//
        + "<caption  align=\"bottom\"><b>Legende</b></caption>"//
        + "<tr>"//
        + "<td bgcolor=\"" + GREEN + "\">bereits reduzierter Abfluss</td>"//
        + "<tr>"//
        + "</tr>"//
        + "<td bgcolor=\"" + RED + "\">noch zu reduzierender Abfluss</td>"//
        + "</tr>"//
        + "</table>";
  }

  private static String generateTableHTML( final double maxStatusQuoQ, final double maxPlaningQ, final double maxPlaningAndMeasureQ, final String hqEventId )
  {
    return "<!-- table info -->" //
        + "  <table border=\"0\"  align=\"left\">"//
        + "    <tr>"//
        + generateCells( "Planung", maxPlaningQ, false ) // 
        + generateCells( "Planung", maxPlaningQ, false ) // 
        + "    </tr><tr>"//
        + generateCells( hqEventId, maxStatusQuoQ, true ) //
        + generateCells( "Massnahmen", maxPlaningAndMeasureQ, true ) //
        + "    </tr><tr>"//
        + generateCells( "Belastung", maxPlaningQ - maxStatusQuoQ, true ) //
        + generateCells( "Reduktion", maxPlaningQ - maxPlaningAndMeasureQ, true ) //
        + "    </tr>"//
        + "  </table>";
  }

  private static String generateCells( final String title, final double q, final boolean underline )
  {
    final StringBuffer result = new StringBuffer();
    result.append( "<td><b>" + title + "</b></td><td>" );
    if( underline )
      result.append( "<u>" );
    result.append( q + " m/s" );
    if( underline )
      result.append( "</u>" );
    result.append( "</td>" );
    return result.toString();
  }

  private static String getContainerPath( Feature nodeFeature )
  {
    final String nodeName = (String) nodeFeature.getProperty( "name" );
    return nodeName;
  }

  private static URL getStatusQuo( String hqEventId, Feature resultNode ) throws MalformedURLException
  {
    final URL statusQuoZipURL = KalypsoDSSPlugin.class.getResource( "resources/resultsStatusQuo.zip" );
    final String relativePath = hqEventId + "/Ergebnisse/" + DefaultPathGenerator.generateResultPathFor( resultNode, "name", "qgs", null );
    return new URL( new URL( "jar:" + statusQuoZipURL.toString() + "!/" ), relativePath );
  }

  private static class Combination
  {
    private final Feature m_nodeFE;

    private final String m_hqEventId;

    private Combination( Feature nodeFE, String hqEventId )
    {
      m_nodeFE = nodeFE;
      m_hqEventId = hqEventId;
    }

    public Feature getNodeFeature( )
    {
      return m_nodeFE;
    }

    public String getHQEventId( )
    {
      return m_hqEventId;
    }
  }

}
