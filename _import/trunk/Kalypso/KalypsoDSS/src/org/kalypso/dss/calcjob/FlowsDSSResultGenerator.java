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
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.tokenreplace.DefaultTokenReplacer;
import org.kalypso.commons.tokenreplace.ITokenReplacer;
import org.kalypso.commons.tokenreplace.TokenReplacerEngine;
import org.kalypso.convert.namodel.DefaultPathGenerator;
import org.kalypso.dss.KalypsoDSSPlugin;
import org.kalypso.dss.utils.MeasuresConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.util.ZMLUtilities;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

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
      return hqEventId + " / Knoten " + nodeNme;
    }
  };

  private static final String BGColor = "#FFFFFF";

  /**
   * @param noMeasures
   *          true, if calculation is based only on planing without measures
   */
  public static List<HTMLFragmentBean> generateDssResultFor( final File dssResultDir, final File rrmResultDir, final ISimulationDataProvider inputProvider, final String hqEventId, final Feature resultNode, final boolean doMeasures, List<HTMLFragmentBean> htmlFragmentCollector ) throws MalformedURLException
  {
    if( htmlFragmentCollector == null )
      htmlFragmentCollector = new ArrayList<HTMLFragmentBean>();
    final String nodeName = (String) resultNode.getProperty( "name" );

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
        if( inputProvider.hasID( MeasuresConstants.IN_LastResults ) )
        {
          final URL lastResultContainerURL = (URL) inputProvider.getInputForID( MeasuresConstants.IN_LastResults );
          srcPlanURL = new URL( lastResultContainerURL, "Ergebnisse/" + hqEventId + "/" + containerPath + "/mitPlanung.zml" );
        }
        else
          srcPlanURL = null;
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

    if( gotStatusQuo && gotPlaning )
    {
      String nodeTitle = createNodeTitle( resultNode );
      final String pageHTML = generateHTML( nodeTitle, nodeName, maxStatusQuoQ, maxPlaningQ, maxPlaningAndMeasureQ, hqEventId, htmlFragmentCollector, gotPlaningAndMeasure );
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
      finally
      {
        IOUtils.closeQuietly( htmlOutputStream );
      }
    }

    // do odt stuff
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

    final List<ITokenReplacer> replacer = new ArrayList<ITokenReplacer>();

    final ITokenReplacer statusQuoReplacer;
    final ITokenReplacer planingReplacer;
    final ITokenReplacer planingAndMeasureReplacer;

    if( gotStatusQuo )
      statusQuoReplacer = new DefaultTokenReplacer( "statusQuoFragment", FlowsOdtFragments.STATUS_QUO );
    else
      statusQuoReplacer = new DefaultTokenReplacer( "statusQuoFragment", "" );

    if( gotPlaning )
      planingReplacer = new DefaultTokenReplacer( "planingFragment", FlowsOdtFragments.PLANING );
    else
      planingReplacer = new DefaultTokenReplacer( "planingFragment", "" );

    if( gotPlaningAndMeasure )
      planingAndMeasureReplacer = new DefaultTokenReplacer( "planingAndMeasureFragment", FlowsOdtFragments.PLANING_AND_MEASURE );
    else
      planingAndMeasureReplacer = new DefaultTokenReplacer( "planingAndMeasureFragment", "" );

    replacer.add( titleReplacer );
    replacer.add( statusQuoReplacer );
    replacer.add( planingReplacer );
    replacer.add( planingAndMeasureReplacer );

    final ITokenReplacer[] replacerArray = replacer.toArray( new ITokenReplacer[replacer.size()] );
    final TokenReplacerEngine replaceEngine = new TokenReplacerEngine( replacerArray );

    // StreamUtilities.streamCopy(resource.openStream(), writer);
    final Combination value = new Combination( resultNode, hqEventId );
    odtAsString = replaceEngine.replaceTokens( value, odtAsString );

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

    return htmlFragmentCollector;
  }

  private static String createNodeTitle( Feature nodeFE )
  {
    final String nodeName = "Knoten " + (String) nodeFE.getProperty( "name" );
    try
    {
      final GM_Object position = (GM_Object) nodeFE.getProperty( "Ort" );
      final GM_Point centroid = position.getCentroid();
      double x = centroid.getPosition().getX();
      double y = centroid.getPosition().getY();
      final String crsName = centroid.getCoordinateSystem().getName();
      final String showInMap = "kalypso://showInMap?title=" + nodeName//
          + "&duration=4000" //
          + "&x=" + x//
          + "&y=" + y// 
          + "&crs=" + crsName;
      return "<a href=\"" + showInMap + "\">" + nodeName + "</a>";
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return nodeName;
  }

  private static String generateHTML( String nodeTitle, String nodeName, double maxStatusQuoQ, double maxPlaningQ, double maxPlaningAndMeasureQ, String hqEventId, final List<HTMLFragmentBean> htmlFragmentCollector, boolean gotPlaningAndMeasure )
  {
    double overLoadQ = maxPlaningQ - maxStatusQuoQ;
    double reducedQ = maxPlaningQ - maxPlaningAndMeasureQ;
    final int reducedPercent;
    if( gotPlaningAndMeasure )
    {
      if( maxPlaningQ == maxStatusQuoQ )
        reducedPercent = 100;
      else
        reducedPercent = (int) (100d * reducedQ / overLoadQ);
    }
    else
    {
      if( maxPlaningQ <= maxStatusQuoQ )
        reducedPercent = 100;
      else
        reducedPercent = 0;
    }
    final String barHTML = generateBarHTML( reducedPercent );
    final String tableHTML = generateTableHTML( maxStatusQuoQ, maxPlaningQ, maxPlaningAndMeasureQ, hqEventId, gotPlaningAndMeasure );
    final String legendHTML = generateLegendHTMLPart();

    final String prefix = hqEventId + "/" + nodeName;
    final String detailedLink = prefix + "/analyse.html";
    final String openODTLink = "kalypso://openEditor?input=" + prefix + "/analyse.odt&activate=true;";
    final String closeODTLink = "kalypso://closeEditor?input=" + prefix + "/analyse.odt&activate=true;";

    // kalypso://openEditor?input=analyse.odt&activate=true;
    // kalypso://closeEditor?input=analyse.odt&doSave=false;

    final String fragment = "<td>" + barHTML + "</td>"//  
        + "<td><a href=\"" + detailedLink + "\">info</a>,"//
        + " Diagram  <a href=\"" + openODTLink + "\">zeigen</a>"//
        + "|<a href=\"" + closeODTLink + "\">schliessen</a></td>";

    htmlFragmentCollector.add( new HTMLFragmentBean( hqEventId, nodeTitle, fragment ) );
    StringBuffer result = new StringBuffer( "<html><body bgcolor=\"" + BGColor + "\">" );

    result.append( " <table width=\"100%\"><tr><td align=\"left\">" );
    result.append( " <b>Details zur Analyse: " + hqEventId + " / " + nodeTitle + "</b>" );
    result.append( " </td><td align=\"right\">" );
    result.append( "<a href=\"../../analyse.html\">zur Hauptseite</a>" );
    result.append( " </td></tr></table>" );
    result.append( "<br>" );
    result.append( "Hochwasservertr‰glichkeit:" );
    result.append( barHTML );
    result.append( "<br>" );
    result.append( "Diagramm <a href=\"" );
    result.append( "kalypso://openEditor?input=analyse.odt&activate=true;" );
    result.append( "\">anzeigen</a> | <a href=\"" );
    result.append( "kalypso://closeEditor?input=analyse.odt&doSave=false" );
    result.append( "\">schliessen</a>" );
    result.append( "<br>" );
    result.append( tableHTML );
    result.append( legendHTML );
    result.append( "</body></html>" );
    return result.toString();
  }

  public static void generateHTMLFormFragments( File targetFile, List<HTMLFragmentBean> fragments, boolean nodeSorted )
  {
    final StringBuffer result = new StringBuffer( "<html><body bgcolor=\"" + BGColor + "\">" );

    final Hashtable<String, HTMLFragmentBean> matrix = new Hashtable<String, HTMLFragmentBean>();

    final SortedSet<String> hqNameSet = new TreeSet<String>( new Comparator<String>()
    {

      public int compare( String o1, String o2 )
      {
        try
        {
          final Integer i1 = new Integer( o1.replaceAll("HQ","") );
          final Integer i2 = new Integer( o2.replaceAll("HQ","") );
          return i1.compareTo( i2 );
        }
        catch( Exception e )
        {
          return o1.compareTo( o2 );
        }
      }

    } );
    final SortedSet<String> nodeTitleSet = new TreeSet<String>();

    for( HTMLFragmentBean fragment : fragments )
    {
      final String hq = fragment.getHqIdentifier();
      final String nodeTitle = fragment.getNodeTitle();
      hqNameSet.add( hq );
      nodeTitleSet.add( nodeTitle );
      final String key = hq + "," + nodeTitle;
      matrix.put( key, fragment );
    }
    final SortedSet<String> set1;
    final SortedSet<String> set2;
    if( nodeSorted )
    {
      set1 = nodeTitleSet;
      set2 = hqNameSet;
    }
    else
    {
      set1 = hqNameSet;
      set2 = nodeTitleSet;
    }

    result.append( " <table width=\"100%\"><tr><td align=\"left\">" );
    result.append( " <b>Analyse der Ergebnisse</b>" );
    result.append( " </td><td align=\"right\">" );
    if( nodeSorted )
      result.append( "<a href=\"./analyseHQ.html\"/>(nach Abfluss sortieren)</a>" );
    else
      result.append( "<a href=\"./analyse.html\"/>(nach Knoten sortieren)</a>" );
    result.append( " </td></tr></table>" );
    result.append( "</br>" );
    for( final String tableTitle : set1 )
    {
      result.append( "<table width=\"100%\">" );
      result.append( "   <colgroup>"//
          + "     <col width=\"5%\">"//
          + "     <col width=\"90%\">"//
          + "     <col width=\"5%\">"//
          + "   </colgroup>" );
      result.append( " <caption  align=\"top\"><b>" + tableTitle + "</b></caption>" );
      for( final String rowTitle : set2 )
      {
        final String key1 = rowTitle + "," + tableTitle;
        final String key2 = tableTitle + "," + rowTitle;
        final HTMLFragmentBean fragment;
        if( matrix.containsKey( key1 ) )
          fragment = matrix.get( key1 );
        else
          fragment = matrix.get( key2 );
        result.append( "<tr><td><b>" + rowTitle + "</b></td>" );
        result.append( fragment.getHtmlFragment() );
        result.append( "</tr>" );
      }

      result.append( "</table>" );
    }
    result.append( generateLegendHTMLPart() );
    result.append( "</html></body>" );
    final String content = result.toString();
    OutputStream htmlOutputStream = null;
    try
    {
      htmlOutputStream = new FileOutputStream( targetFile );
      IOUtils.write( content, htmlOutputStream );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( htmlOutputStream );
    }
  }

  private final static String GREEN = "#339933";

  private final static String RED = "#CC3333";

  private static String generateBarHTML( final int reducedPercent )
  {
    StringBuffer result = new StringBuffer();
    result.append( "<table border=\"0\" width=\"100%\" height=\"30\">" );
    result.append( "   <colgroup>" );
    result.append( "     <col width=\"" + (100 - reducedPercent) + "%\">" );
    result.append( "     <col width=\"" + reducedPercent + "%\">" );
    result.append( "   </colgroup>" );
    result.append( "   <tr>" );

    if( reducedPercent == 100 )
      result.append( "     <td bgcolor=\"" + GREEN + "\"></td>" );
    else if( reducedPercent == 0 )
      result.append( "     <td bgcolor=\"" + RED + "\"></td>" );
    else
      result.append( "     <td bgcolor=\"" + RED + "\"></td>" );

    if( reducedPercent == 100 )
      result.append( "     <td bgcolor=\"" + GREEN + "\"></td>" );
    else if( reducedPercent == 0 )
      result.append( "     <td bgcolor=\"" + RED + "\"></td>" );
    else
      result.append( "     <td bgcolor=\"" + GREEN + "\"></td>" );

    result.append( "   </tr>" );//
    result.append( "</table>" );
    return result.toString();
  }

  private static String generateLegendHTMLPart( )
  {
    return "<!-- legend -->"//
        + "<table border=\"2\" align=\"right\">"//
        + "<caption  align=\"bottom\"><b>Legende</b></caption>"//
        + "<tr>"//
        + "<td bgcolor=\"" + GREEN + "\">Abflussreduktion durch Massnahmen</td>"//
        + "<tr>"//
        + "</tr>"//
        + "<td bgcolor=\"" + RED + "\">Abflusszunahme durch Planung</td>"//
        + "</tr>"//
        + "</table>";
  }

  private static String generateTableHTML( final double maxStatusQuoQ, final double maxPlaningQ, final double maxPlaningAndMeasureQ, final String hqEventId, boolean gotPlaningAndMeasure )
  {

    final StringBuffer result = new StringBuffer( "<!-- table info -->" );
    result.append( "  <table border=\"0\"  align=\"left\">" );
    result.append( "    <tr>" );
    result.append( generateCells( "Planung", maxPlaningQ, false ) );
    if( gotPlaningAndMeasure )
      result.append( generateCells( "Planung", maxPlaningQ, false ) );
    result.append( "    </tr><tr>" );
    result.append( generateCells( hqEventId, maxStatusQuoQ, true ) );
    if( gotPlaningAndMeasure )
      result.append( generateCells( "Massnahmen", maxPlaningAndMeasureQ, true ) );
    result.append( "    </tr><tr>" );
    final double delta1 = maxPlaningQ - maxStatusQuoQ;
    result.append( generateCells( "zus. Belastung", delta1, false ) );
    if( gotPlaningAndMeasure )
    {
      final double delta2 = maxPlaningQ - maxPlaningAndMeasureQ;
      result.append( generateCells( "Reduktion", delta2, false ) );

    }
    result.append( "    </tr>" );
    result.append( "  </table>" );
    return result.toString();
  }

  private static String generateCells( final String title, final double q, final boolean underline )
  {
    final NumberFormat nFormat = NumberFormat.getInstance();
    nFormat.setMinimumFractionDigits( 2 );
    nFormat.setMaximumFractionDigits( 4 );
    final String qString = nFormat.format( q );

    final StringBuffer result = new StringBuffer();
    result.append( "<td><b>" + title + "</b></td><td>" );
    if( underline )
      result.append( "<u>" );
    result.append( qString + " qm/s" );
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
