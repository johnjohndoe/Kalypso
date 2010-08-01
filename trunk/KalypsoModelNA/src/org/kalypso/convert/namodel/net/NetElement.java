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
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.net.visitors.NetElementVisitor;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * A NetElement encapsulates a Channel-Element and its dependencies <br>
 * In the example below each channel represents one net-element. Here you can see the dependencies related to the one
 * channel written in capital letter in the middle.
 * 
 * <pre>
 *                                          Node-----&gt;-----Node
 *                                            |              |
 *                                            V              V
 *                                            |              |
 *                         Catchment---&gt;---CHANNEL        Channel (downstream)
 *                             |              |           
 *                             |              V           
 *                             V              |           
 *                             |             Node
 *                             |              |
 *                          Catchment         V
 *                             |              |
 *                             V           Channel (downstream)
 *                             |
 *                          Channel (downstream)
 * 
 * 
 * </pre>
 */
public class NetElement
{
  private final static String FILTER_T = "<filter><intervallFilter amount=\"24\" calendarField=\"HOUR_OF_DAY\" mode=\"intensity\" xmlns=\"filters.zml.kalypso.org\"/></filter>"; //$NON-NLS-1$

  private final static String FILTER_V = FILTER_T;

  private boolean m_calculated = false;

  private final List<NetElement> m_upStreamDepends = new ArrayList<NetElement>();

  private final List<NetElement> m_downStreamDepends = new ArrayList<NetElement>();

  private final Feature m_channelFE;

  private static final int ANFANGSKNOTEN = 9001; //$NON-NLS-1$

  private static final String ENDKNOTEN = "   10000"; //$NON-NLS-1$

  private final UrlUtilities m_urlUtils = new UrlUtilities();

  private final GMLWorkspace m_synthNWorkspace;

  private final GMLWorkspace m_workspace;

  private final NAConfiguration m_conf;

  public NetElement( final GMLWorkspace modellWorkspace, final GMLWorkspace synthNWorkspace, final Feature channelFE, final NAConfiguration conf )
  {
    m_synthNWorkspace = synthNWorkspace;
    m_channelFE = channelFE;
    m_workspace = modellWorkspace;
    m_conf = conf;
  }

  public Feature getChannel( )
  {
    return m_channelFE;
  }

  public Feature getDownStreamNode( )
  {
    final IRelationType rt = (IRelationType) m_channelFE.getFeatureType().getProperty( NaModelConstants.DOWNSTREAM_NODE_MEMBER_PROP );
    return m_workspace.resolveLink( m_channelFE, rt );
  }

  public boolean isCalculated( )
  {
    return m_calculated;
  }

  public void generateTimeSeries( ) throws IOException, Exception
  {
    final IFeatureType catchmentFT = m_conf.getCatchemtFT();
    final IRelationType rt = (IRelationType) catchmentFT.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
    final Feature[] catchmentFeatures = m_workspace.resolveWhoLinksTo( m_channelFE, catchmentFT, rt );
    for( final Feature feature : catchmentFeatures )
    {
      final File targetFileN = CatchmentManager.getNiederschlagEingabeDatei( feature, new File( m_conf.getAsciiBaseDir(), "klima.dat" ), m_conf ); //$NON-NLS-1$
      final File targetFileT = CatchmentManager.getTemperaturEingabeDatei( feature, new File( m_conf.getAsciiBaseDir(), "klima.dat" ), m_conf ); //$NON-NLS-1$
      final File targetFileV = CatchmentManager.getVerdunstungEingabeDatei( feature, new File( m_conf.getAsciiBaseDir(), "klima.dat" ), m_conf ); //$NON-NLS-1$
      final File parent = targetFileN.getParentFile();
      if( !parent.exists() )
        parent.mkdirs();

      if( m_conf.isUsePrecipitationForm().equals( true ) )
      {
        if( !targetFileN.exists() )
          CatchmentManager.WriteSynthNFile( targetFileN, feature, m_synthNWorkspace, m_conf );
      }
      else
      {
        // N
        if( !targetFileN.exists() )
        {
          final TimeseriesLinkType linkN = (TimeseriesLinkType) feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_NIEDERSCHLAG );

          final URL linkURLN = m_urlUtils.resolveURL( m_conf.getZMLContext(), linkN.getHref() );
          final IObservation observation = ZmlFactory.parseXML( linkURLN, "ID_N" ); //$NON-NLS-1$
          final FileWriter writer = new FileWriter( targetFileN );
          NAZMLGenerator.createFile( writer, TimeserieConstants.TYPE_RAINFALL, observation );
          IOUtils.closeQuietly( writer );
        }
        // T
        if( !targetFileT.exists() )
        {
          final TimeseriesLinkType linkT = (TimeseriesLinkType) feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_TEMPERATUR );
          if( linkT != null )
          {
            final String hrefT = ZmlURL.insertFilter( linkT.getHref(), FILTER_T );
            final URL linkURLT = m_urlUtils.resolveURL( m_conf.getZMLContext(), hrefT );
            final IObservation observation = ZmlFactory.parseXML( linkURLT, "ID_T" ); //$NON-NLS-1$
            final FileWriter writer = new FileWriter( targetFileT );
            NAZMLGenerator.createExt2File( writer, observation, m_conf.getSimulationStart(), m_conf.getSimulationEnd(), TimeserieConstants.TYPE_TEMPERATURE, "1.0" ); //$NON-NLS-1$
            IOUtils.closeQuietly( writer );
          }
        }
        // V
        if( !targetFileV.exists() )
        {
          final TimeseriesLinkType linkV = (TimeseriesLinkType) feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_VERDUNSTUNG );
          if( linkV != null )
          {
            final String hrefV = ZmlURL.insertFilter( linkV.getHref(), FILTER_V );
            final URL linkURLV = m_urlUtils.resolveURL( m_conf.getZMLContext(), hrefV );
            final IObservation observation = ZmlFactory.parseXML( linkURLV, "ID_V" ); //$NON-NLS-1$
            final FileWriter writer = new FileWriter( targetFileV );
            NAZMLGenerator.createExt2File( writer, observation, m_conf.getSimulationStart(), m_conf.getSimulationEnd(), TimeserieConstants.TYPE_EVAPORATION, "0.5" ); //$NON-NLS-1$
            IOUtils.closeQuietly( writer );
          }
        }
      }
    }
  }

  private void addDownStream( final NetElement downStreamElement )
  {
    if( !m_downStreamDepends.contains( downStreamElement ) )
      m_downStreamDepends.add( downStreamElement );
  }

  public Feature getChannelsBelowDownStreamNode( )
  {
    final Feature downStreamNode = getDownStreamNode();
    final IRelationType rt = (IRelationType) downStreamNode.getFeatureType().getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
    return m_workspace.resolveLink( downStreamNode, rt );
  }

  public List<NetElement> getDownStreamNetElements( )
  {
    return m_downStreamDepends;
  }

  public List<NetElement> getUpStreamNetElements( )
  {
    return m_upStreamDepends;
  }

  public void addUpStream( final NetElement upStreamElement )
  {
    if( !m_upStreamDepends.contains( upStreamElement ) )
      m_upStreamDepends.add( upStreamElement );
    upStreamElement.addDownStream( this );
  }

  public void writeRootChannel( final AsciiBuffer asciiBuffer, final int virtualChannelId )
  {
    final StringBuffer netBuffer = asciiBuffer.getNetBuffer();
    final StringBuffer channelBuffer = asciiBuffer.getChannelBuffer();

    final IDManager idManager = m_conf.getIdManager();

    final IRelationType rt = (IRelationType) m_channelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
    final Feature knotu = m_workspace.resolveLink( m_channelFE, rt );
    if( knotu == null )
      System.out.println( "knotU=null" ); //$NON-NLS-1$

    netBuffer.append( "   " + virtualChannelId ); //$NON-NLS-1$
    netBuffer.append( String.format( "%8d", idManager.getAsciiID( knotu ) ) ); //$NON-NLS-1$
    netBuffer.append( ENDKNOTEN );
    netBuffer.append( " 0\n" ); //$NON-NLS-1$

    channelBuffer.append( virtualChannelId + "\n" ); //$NON-NLS-1$
    channelBuffer.append( ChannelManager.VIRTUALCHANNEL + "\n" ); //$NON-NLS-1$
  }

  /**
   * writes part 1 of netfile
   */
  public void write( final AsciiBuffer asciiBuffer, final List<Feature> nodeList )
  {
    final Feature channel = getChannel();
    asciiBuffer.markFeatureForWrite( channel );

    m_calculated = true;

    final StringBuffer netBuffer = asciiBuffer.getNetBuffer();

    final IDManager idManager = m_conf.getIdManager();

    // append channel:
    final int channelID = idManager.getAsciiID( m_channelFE );
    netBuffer.append( String.format( "%8d", channelID ) ); //$NON-NLS-1$

    final IRelationType rt = (IRelationType) m_channelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
    final Feature downstreamNode = m_workspace.resolveLink( m_channelFE, rt );
    final Feature upstreamNode = findUpstreamNode();

    // collect related catchments
    final IFeatureType catchemtFT = m_conf.getCatchemtFT();
    final Feature[] catchmentFeatures = m_workspace.getFeatures( catchemtFT );
    final Feature[] catchmentForThisChannel = findCatchments( catchemtFT, catchmentFeatures );

    // append upstream node:
    final int upstreamNodeID = upstreamNode == null ? ANFANGSKNOTEN : idManager.getAsciiID( upstreamNode );
    netBuffer.append( String.format( "%8d", upstreamNodeID ) );

    // append downstream node:
    final int downstreamNodeID = idManager.getAsciiID( downstreamNode );
    netBuffer.append( String.format( "%8d", downstreamNodeID ) ); //$NON-NLS-1$

    // append catchments
    netBuffer.append( " " + catchmentForThisChannel.length + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    for( final Object element : catchmentForThisChannel )
    {
      final Feature catchmentFE = (Feature) element;
      asciiBuffer.markFeatureForWrite( catchmentFE );
      final int chatchmentID = idManager.getAsciiID( catchmentFE );
      netBuffer.append( String.format( "%8d\n", chatchmentID ) ); //$NON-NLS-1$
    }

    if( upstreamNode != null && !nodeList.contains( upstreamNode ) )
      nodeList.add( upstreamNode );
    if( downstreamNode != null && !nodeList.contains( downstreamNode ) )
      nodeList.add( downstreamNode );
  }

  // FIXME:move into channel binding
  private Feature[] findCatchments( final IFeatureType catchemtFT, final Feature[] catchmentFeatures )
  {
    final List<Feature> catchmentList = new ArrayList<Feature>();

    final IRelationType entwaesserungsStrangMemberRT = (IRelationType) catchemtFT.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
    for( final Feature cfeature : catchmentFeatures )
    {
      final Feature channel = m_workspace.resolveLink( cfeature, entwaesserungsStrangMemberRT );
      if( m_channelFE == channel )
        catchmentList.add( cfeature );
    }

    return catchmentList.toArray( new Feature[catchmentList.size()] );
  }

  // FIXME:move into channel binding
  private Feature findUpstreamNode( )
  {
    final IFeatureType nodeFT = m_conf.getNodeFT();
    final Feature[] features = m_workspace.getFeatures( nodeFT );
    final IRelationType downStreamChannelMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
    for( final Feature node : features )
    {
      final Feature downStreamChannel = m_workspace.resolveLink( node, downStreamChannelMemberRT );
      if( m_channelFE == downStreamChannel )
        return node;
    }
    return null;
  }

  public void accept( final NetElementVisitor visitor ) throws Exception
  {
    visitor.visit( this );
  }

  public GMLWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  @Override
  public String toString( )
  {
    final Feature channel = getChannel();
    return "FID:" + channel.getId() + " AsciiID: " + m_conf.getIdManager().getAsciiID( channel ); //$NON-NLS-1$ //$NON-NLS-2$
  }
}