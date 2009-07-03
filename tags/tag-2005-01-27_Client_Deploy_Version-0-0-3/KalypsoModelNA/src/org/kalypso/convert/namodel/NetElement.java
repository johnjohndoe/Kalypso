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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.kalypso.convert.ASCIIHelper;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLink;

public class NetElement
{
  private final NetFileManager m_manager;

  public final static int UNCALCULATED = 1;

  public final static int CALCULATED = 2;

  private final List m_upStreamDepends = new ArrayList();

  private final List m_downStreamDepends = new ArrayList();

  private final Feature m_channelFE;

  private static final String ANFANGSKNOTEN = "    9001";

  private static final String ENDKNOTEN = "   10000";

  private int m_status = UNCALCULATED;

  private final UrlUtilities m_urlUtils = new UrlUtilities();

  private static String[] m_netAsciiFormat;

  private static GMLWorkspace m_workspace;

  public static void setNetAsciiFormats( String[] formats )
  {
    m_netAsciiFormat = formats;
  }

  public static void setDefaultGmlWorkSpace( GMLWorkspace workspace )
  {
    m_workspace = workspace;
  }

  public NetElement( NetFileManager manager, Feature channelFE )
  {
    m_channelFE = channelFE;
    this.m_manager = manager;
  }

  public Feature getChannel()
  {
    return m_channelFE;
  }

  public boolean isCalculated()
  {
    return m_status == CALCULATED;
  }

  public boolean resultExists()
  {
    final Feature knotU = m_workspace.resolveLink( m_channelFE, "downStreamNodeMember" );
    return m_manager.resultExists( knotU );
  }

  /**
   * @param isRootElement
   *          upstream from root node, only first channel is not upstream from
   *          rootnode
   */
  public void berechne( AsciiBuffer asciiBuffer, List nodeList, boolean isRootElement )
      throws IOException, Exception
  {
    if( isCalculated() )
      return;

    // if result does not exists, calculate upstrem
    boolean resultExists = resultExists();
    if( !resultExists )
      berechneOberlauf( asciiBuffer, nodeList );

    // calculate me
    write( asciiBuffer, nodeList, resultExists, isRootElement );
    generateTimeSeries();
    m_status = CALCULATED;
  }

  public void berechneOberlauf( AsciiBuffer asciiBuffer, List nodeList ) throws Exception
  {
    for( Iterator iter = m_upStreamDepends.iterator(); iter.hasNext(); )
    {
      // berechne oberlauf
      NetElement element = (NetElement)iter.next();
      element.berechne( asciiBuffer, nodeList, false );
    }
  }

  private void generateTimeSeries() throws IOException, Exception
  {
    Feature[] catchmentFeatures = m_workspace.resolveWhoLinksTo( m_channelFE, this.m_manager.m_conf
        .getCatchemtFT(), "entwaesserungsStrangMember" );
    for( int i = 0; i < catchmentFeatures.length; i++ )
    {
      final Feature feature = catchmentFeatures[i];
      final TimeseriesLink link = (TimeseriesLink)feature.getProperty( "niederschlagZR" );
      final URL linkURL = m_urlUtils.resolveURL( this.m_manager.m_conf.getGMLModelURL(), link
          .getHref() );
      final String tsFileName = CatchmentManager.getNiederschlagEingabeDateiString( feature );
      final File targetFile = new File( this.m_manager.m_conf.getAsciiBaseDir(), "klima.dat/"
          + tsFileName );
      final File parent = targetFile.getParentFile();
      if( !parent.exists() )
        parent.mkdirs();

      if( !NetFileManager.DEBUG )
      {

        final IObservation observation = ZmlFactory.parseXML( linkURL, "ID" );
        final FileWriter writer = new FileWriter( targetFile );
        NAZMLGenerator.createFile( writer, TimeserieConstants.TYPE_RAINFALL, observation );
        writer.close();
      }
    }
  }

  private void addDownStream( NetElement downStreamElement )
  {
    if( !m_downStreamDepends.contains( downStreamElement ) )
      m_downStreamDepends.add( downStreamElement );
  }

  public List getDownStreamNetElements()
  {
    return m_downStreamDepends;
  }

  public void addUpStream( NetElement upStreamElement )
  {
    if( !m_upStreamDepends.contains( upStreamElement ) )
      m_upStreamDepends.add( upStreamElement );
    upStreamElement.addDownStream( this );
  }

  public void writeRootChannel( AsciiBuffer asciiBuffer, List processedRootNodes )
  {
    final Feature knotO = m_workspace.resolveLink( m_channelFE, "downStreamNodeMember" );
    if( !processedRootNodes.contains( knotO ) )
    {
      processedRootNodes.add( knotO );
      int channelID = 10000 + processedRootNodes.size();
      asciiBuffer.getNetBuffer().append( "   " + channelID );
      asciiBuffer.getNetBuffer().append( ASCIIHelper.toAsciiLine( knotO, m_netAsciiFormat[11] ) );
      asciiBuffer.getNetBuffer().append( ENDKNOTEN );
      asciiBuffer.getNetBuffer().append( " 0\n" );

      asciiBuffer.getChannelBuffer().append( channelID + "\n" );
      asciiBuffer.getChannelBuffer().append( ChannelManager.VIRTUALCHANNEL + "\n" );
    }
  }

  /**
   * writes part 1 of netfile
   * 
   * @param isRootElement
   *          false: this is a source node
   */
  public void write( AsciiBuffer asciiBuffer, List nodeList, boolean resultExists,
      boolean isRootElement )
  {
    asciiBuffer.addFeatureToWrite( getChannel() );
    final Feature knotU = m_workspace.resolveLink( m_channelFE, "downStreamNodeMember" );

    //  append channel:
    asciiBuffer.getNetBuffer()
        .append( ASCIIHelper.toAsciiLine( m_channelFE, m_netAsciiFormat[12] ) );

    // append upstream node:
    //    if( !resultExists || isRootElement )
    //    {
    // find upstream node
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
    // collect related catchments
    final List catchmentList = new ArrayList();
    final Feature[] Cfeatures = m_workspace.getFeatures( this.m_manager.m_conf.getCatchemtFT() );
    for( int i = 0; i < Cfeatures.length; i++ )
    {
      if( m_channelFE == m_workspace.resolveLink( Cfeatures[i], "entwaesserungsStrangMember" ) )
        catchmentList.add( Cfeatures[i] );
    }

    // append upstream node:
    if( knotO != null && !resultExists )
      asciiBuffer.getNetBuffer().append( ASCIIHelper.toAsciiLine( knotO, m_netAsciiFormat[11] ) );
    else
      asciiBuffer.getNetBuffer().append( ANFANGSKNOTEN );

    // append downstream node:
    asciiBuffer.getNetBuffer().append( ASCIIHelper.toAsciiLine( knotU, m_netAsciiFormat[11] ) );

    // append catchments
    if( !resultExists )
    {
      asciiBuffer.getNetBuffer().append( " " + catchmentList.size() + "\n" );
      for( Iterator iter = catchmentList.iterator(); iter.hasNext(); )
      {
        Feature catchmentFE = (Feature)iter.next();
        asciiBuffer.addFeatureToWrite( catchmentFE );
        asciiBuffer.getNetBuffer().append(
            ASCIIHelper.toAsciiLine( catchmentFE, m_netAsciiFormat[12] ) + "\n" );
      }
    }
    else
    {
      asciiBuffer.getNetBuffer().append( " 0\n" ); // simulate no catchments
    }
    // unterhalb des rootnodes letzter strang zum endknoten
    // ohne teilgebiete
    //    if( isRootElement )
    //    {
    //      // end channel
    //// buffer.append( ENDSTRANG );
    //      buffer.append(" "+1000xxx);
    //      // upstream node
    //      buffer.append( ASCIIHelper.toAsciiLine( knotU, m_netAsciiFormat[11] ) );
    //      // downstream node
    //      buffer.append( ENDKNOTEN );
    //      buffer.append( " 0\n" ); // no catchments
    //    }
    if( knotO != null && !nodeList.contains( knotO ) )
      nodeList.add( knotO );
    //    }
    //    else
    //    // result exists
    //    {
    //      buffer.append( ANFANGSKNOTEN );
    //      buffer.append( ASCIIHelper.toAsciiLine( knotU, m_netAsciiFormat[11] ) );
    //      buffer.append( " 0\n" ); // no catchments
    //    }
    if( knotU != null && !nodeList.contains( knotU ) )
      nodeList.add( knotU );
  }
}