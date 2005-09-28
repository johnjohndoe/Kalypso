/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.net;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.net.visitors.NetElementVisitor;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * A NetElement encapsulates a Channel-Element and its dependencies <br>
 * In the example below each channel represents one netelement. Here you can see the dependencies related the one
 * channel written in capital letter in the middle.
 * 
 * <pre>
 *                    Node----->-----Node
 *                      |              |
 *                      V              V
 *                      |              |
 *   Catchment--->---CHANNEL        Channel (downstream)
 *       |              |           
 *       |              V           
 *       V              |           
 *       |             Node
 *       |              |
 *    Catchment         V
 *       |              |
 *       V           Channel (downstream)
 *       |
 *    Channel (downstream)
 * 
 * 
 * </pre>
 * 
 *  
 */
public class NetElement
{
  private final static String FILTER_T = "<filter><intervallFilter amount=\"24\" calendarField=\"HOUR_OF_DAY\" mode=\"intensity\" xmlns=\"filters.zml.kalypso.org\"/></filter>";

  private final static String FILTER_V = FILTER_T;

  private boolean m_calculated = false;

  private final NetFileManager m_manager;

  private final List m_upStreamDepends = new ArrayList();

  private final List m_downStreamDepends = new ArrayList();

  private final Feature m_channelFE;

  private static final String ANFANGSKNOTEN = "    9001";

  private static final String ENDKNOTEN = "   10000";

  private final UrlUtilities m_urlUtils = new UrlUtilities();

  //  private static String[] m_netAsciiFormat;

  private final GMLWorkspace m_workspace;


  private final NAConfiguration m_conf;

  public NetElement( NetFileManager manager, GMLWorkspace modellWorkspace, Feature channelFE, NAConfiguration conf )
  {
    m_channelFE = channelFE;
    m_manager = manager;
    m_workspace = modellWorkspace;
    m_conf = conf;
  }

  public Feature getChannel()
  {
    return m_channelFE;
  }

  public Feature getDownStreamNode()
  {
    return m_workspace.resolveLink( m_channelFE, "downStreamNodeMember" );
  }

  public boolean isCalculated()
  {
    return m_calculated;
  }

  /**
   *  
   */
  public void berechne( AsciiBuffer asciiBuffer, List nodeList ) throws IOException, Exception
  {
    if( isCalculated() )
      return;

    berechneOberlauf( asciiBuffer, nodeList );

    // calculate me
    write( asciiBuffer, nodeList );
    generateTimeSeries();

  }

  public void berechneOberlauf( AsciiBuffer asciiBuffer, List nodeList ) throws Exception
  {
    for( Iterator iter = m_upStreamDepends.iterator(); iter.hasNext(); )
    {
      // berechne oberlauf
      NetElement element = (NetElement)iter.next();
      element.berechne( asciiBuffer, nodeList );
    }
  }

  public void generateTimeSeries() throws IOException, Exception
  {
    final Feature[] catchmentFeatures = m_workspace.resolveWhoLinksTo( m_channelFE, m_manager.m_conf.getCatchemtFT(),
        "entwaesserungsStrangMember" );
    for( int i = 0; i < catchmentFeatures.length; i++ )
    {
      final NAConfiguration conf = m_manager.m_conf;
      final Feature feature = catchmentFeatures[i];

      final File targetFileN = CatchmentManager.getNiederschlagEingabeDatei( feature, new File( conf.getAsciiBaseDir(),
          "klima.dat" ), conf );
      final File targetFileT = CatchmentManager.getTemperaturEingabeDatei( feature, new File( m_manager.m_conf
          .getAsciiBaseDir(), "klima.dat" ), conf );
      final File targetFileV = CatchmentManager.getVerdunstungEingabeDatei( feature, new File( m_manager.m_conf
          .getAsciiBaseDir(), "klima.dat" ), conf );
      final File parent = targetFileN.getParentFile();
      if( !parent.exists() )
        parent.mkdirs();

      // N
      if( !targetFileN.exists() )
      {
        final TimeseriesLink linkN = (TimeseriesLink)feature.getProperty( "niederschlagZR" );
        final URL linkURLN = m_urlUtils.resolveURL( m_workspace.getContext(), linkN.getHref() );
        final IObservation observation = ZmlFactory.parseXML( linkURLN, "ID_N" );
        final FileWriter writer = new FileWriter( targetFileN );
        NAZMLGenerator.createFile( writer, TimeserieConstants.TYPE_RAINFALL, observation );
        IOUtils.closeQuietly( writer );
      }
      // T
      if( !targetFileT.exists() )
      {
        final TimeseriesLink linkT = (TimeseriesLink)feature.getProperty( "temperaturZR" );
        if( linkT != null )
        {
          final String hrefT = ZmlURL.insertFilter( linkT.getHref(), FILTER_T );
          final URL linkURLT = m_urlUtils.resolveURL( m_workspace.getContext(), hrefT );
          final IObservation observation = ZmlFactory.parseXML( linkURLT, "ID_T" );
          final FileWriter writer = new FileWriter( targetFileT );
          NAZMLGenerator.createExt2File( writer, observation, conf.getSimulationStart(), conf.getSimulationEnd(),
              TimeserieConstants.TYPE_TEMPERATURE, "1.0" );
          IOUtils.closeQuietly( writer );
        }
      }
      // V
      if( !targetFileV.exists() )
      {
        final TimeseriesLink linkV = (TimeseriesLink)feature.getProperty( "verdunstungZR" );
        if( linkV != null )
        {
          final String hrefV = ZmlURL.insertFilter( linkV.getHref(), FILTER_V );
          final URL linkURLV = m_urlUtils.resolveURL( m_workspace.getContext(), hrefV );
          final IObservation observation = ZmlFactory.parseXML( linkURLV, "ID_V" );
          final FileWriter writer = new FileWriter( targetFileV );
          NAZMLGenerator.createExt2File( writer, observation, conf.getSimulationStart(), conf.getSimulationEnd(),
              TimeserieConstants.TYPE_EVAPORATION, "0.5" );
          IOUtils.closeQuietly( writer );
        }
      }
    }
  }

  private void addDownStream( NetElement downStreamElement )
  {
    if( !m_downStreamDepends.contains( downStreamElement ) )
      m_downStreamDepends.add( downStreamElement );
  }

  public Feature getChannelsBelowDownStreamNode()
  {
    return m_workspace.resolveLink( getDownStreamNode(), "downStreamChannelMember" );
  }

  public List getDownStreamNetElements()
  {
    return m_downStreamDepends;
  }

  public List getUpStreamNetElements()
  {
    return m_upStreamDepends;
  }

  public void addUpStream( NetElement upStreamElement )
  {
    if( !m_upStreamDepends.contains( upStreamElement ) )
      m_upStreamDepends.add( upStreamElement );
    upStreamElement.addDownStream( this );
  }

  public void writeRootChannel( AsciiBuffer asciiBuffer, int virtualChannelId )
  {
    final IDManager idManager = m_conf.getIdManager();
    final Feature knotu = m_workspace.resolveLink( m_channelFE, "downStreamNodeMember" );
    if( knotu == null )
      System.out.println( "knotU=null" );
    asciiBuffer.getNetBuffer().append( "   " + virtualChannelId );
    asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( knotu ), "i8" ) );
    //    asciiBuffer.getNetBuffer().append( ASCIIHelper.toAsciiLine( knotu, m_netAsciiFormat[11] ) );
    asciiBuffer.getNetBuffer().append( ENDKNOTEN );
    asciiBuffer.getNetBuffer().append( " 0\n" );

    asciiBuffer.getChannelBuffer().append( virtualChannelId + "\n" );
    asciiBuffer.getChannelBuffer().append( ChannelManager.VIRTUALCHANNEL + "\n" );
    //    m_virtual = true;
  }

  /**
   * writes part 1 of netfile
   *  
   */
  public void write( AsciiBuffer asciiBuffer, List nodeList )
  {
    final IDManager idManager = m_conf.getIdManager();
    asciiBuffer.addFeatureToWrite( getChannel() );
    final Feature knotU = m_workspace.resolveLink( m_channelFE, "downStreamNodeMember" );

    //  append channel:
    asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( m_channelFE ), "i8" ) );

    Feature[] features = m_workspace.getFeatures( m_manager.m_conf.getNodeFT() );
    Feature knotO = null;
    for( int i = 0; i < features.length; i++ )
    {
      if( m_channelFE == m_workspace.resolveLink( features[i], "downStreamChannelMember" ) )
      {
        knotO = features[i];
        continue;
      }
    }
    m_calculated = true;
    // collect related catchments
    final List catchmentList = new ArrayList();
    final Feature[] Cfeatures = m_workspace.getFeatures( m_manager.m_conf.getCatchemtFT() );
    for( int i = 0; i < Cfeatures.length; i++ )
    {
      if( m_channelFE == m_workspace.resolveLink( Cfeatures[i], "entwaesserungsStrangMember" ) )
        catchmentList.add( Cfeatures[i] );
    }

    // append upstream node:
    if( knotO != null )
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( knotO ), "i8" ) );
    else
      asciiBuffer.getNetBuffer().append( ANFANGSKNOTEN );

    // append downstream node:
    asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( knotU ), "i8" ) );

    asciiBuffer.getNetBuffer().append( " " + catchmentList.size() + "\n" );
    for( Iterator iter = catchmentList.iterator(); iter.hasNext(); )
    {
      Feature catchmentFE = (Feature)iter.next();
      asciiBuffer.addFeatureToWrite( catchmentFE );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( catchmentFE ), "i8" ) );
      asciiBuffer.getNetBuffer().append( "\n" );
    }
    if( knotO != null && !nodeList.contains( knotO ) )
      nodeList.add( knotO );
    if( knotU != null && !nodeList.contains( knotU ) )
      nodeList.add( knotU );
  }

  public void accept( NetElementVisitor visitor ) throws Exception
  {
    visitor.visit( this );
  }

  public GMLWorkspace getWorkspace()
  {
    return m_workspace;
  }

  public String toString()
  {
    final Feature channel = getChannel();
    return "FID:" + channel.getId() + " AsciiID: " + m_conf.getIdManager().getAsciiID( channel );
  }
}