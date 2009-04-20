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
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.net.visitors.NetElementVisitor;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * A NetElement encapsulates a Channel-Element and its dependencies <br>
 * In the example below each channel represents one netelement. Here you can see the dependencies related to the one
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

  private final NetFileManager m_manager;

  private final List<NetElement> m_upStreamDepends = new ArrayList<NetElement>();

  private final List<NetElement> m_downStreamDepends = new ArrayList<NetElement>();

  private final Feature m_channelFE;

  private static final String ANFANGSKNOTEN = "    9001"; //$NON-NLS-1$

  private static final String ENDKNOTEN = "   10000"; //$NON-NLS-1$

  private final UrlUtilities m_urlUtils = new UrlUtilities();

  // private static String[] m_netAsciiFormat;

  private final GMLWorkspace m_synthNWorkspace;

  private final GMLWorkspace m_workspace;

  private final NAConfiguration m_conf;

  public NetElement( NetFileManager manager, GMLWorkspace modellWorkspace, GMLWorkspace synthNWorkspace, Feature channelFE, NAConfiguration conf )
  {
    m_synthNWorkspace = synthNWorkspace;
    m_channelFE = channelFE;
    m_manager = manager;
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

  /**
   *  
   */
  public void berechne( AsciiBuffer asciiBuffer, List<Feature> nodeList ) throws IOException, Exception
  {
    if( isCalculated() )
      return;

    berechneOberlauf( asciiBuffer, nodeList );

    // calculate me
    write( asciiBuffer, nodeList );
    generateTimeSeries();

  }

  public void berechneOberlauf( AsciiBuffer asciiBuffer, List<Feature> nodeList ) throws Exception
  {
    for( Iterator iter = m_upStreamDepends.iterator(); iter.hasNext(); )
    {
      // berechne oberlauf
      NetElement element = (NetElement) iter.next();
      element.berechne( asciiBuffer, nodeList );
    }
  }

  public void generateTimeSeries( ) throws IOException, Exception
  {
    final IFeatureType catchmentFT = m_manager.m_conf.getCatchemtFT();
    final IRelationType rt = (IRelationType) catchmentFT.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
    final Feature[] catchmentFeatures = m_workspace.resolveWhoLinksTo( m_channelFE, catchmentFT, rt );
    for( int i = 0; i < catchmentFeatures.length; i++ )
    {
      final NAConfiguration conf = m_manager.m_conf;
      final Feature feature = catchmentFeatures[i];

      final File targetFileN = CatchmentManager.getNiederschlagEingabeDatei( feature, new File( conf.getAsciiBaseDir(), "klima.dat" ), conf ); //$NON-NLS-1$
      final File targetFileT = CatchmentManager.getTemperaturEingabeDatei( feature, new File( m_manager.m_conf.getAsciiBaseDir(), "klima.dat" ), conf ); //$NON-NLS-1$
      final File targetFileV = CatchmentManager.getVerdunstungEingabeDatei( feature, new File( m_manager.m_conf.getAsciiBaseDir(), "klima.dat" ), conf ); //$NON-NLS-1$
      final File parent = targetFileN.getParentFile();
      if( !parent.exists() )
        parent.mkdirs();

      if( conf.isUsePrecipitationForm().equals( true ) )
      {
        if( !targetFileN.exists() )
          CatchmentManager.WriteSynthNFile( targetFileN, feature, m_synthNWorkspace, conf );
      }
      else
      {
        // N
        if( !targetFileN.exists() )
        {
          final TimeseriesLinkType linkN = (TimeseriesLinkType) feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_NIEDERSCHLAG );

          // final URL linkURLN = m_urlUtils.resolveURL( m_workspace.getContext(), linkN.getHref() );
          final URL linkURLN = m_urlUtils.resolveURL( conf.getZMLContext(), linkN.getHref() );
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
            // final URL linkURLT = m_urlUtils.resolveURL( m_workspace.getContext(), hrefT );
            final URL linkURLT = m_urlUtils.resolveURL( conf.getZMLContext(), hrefT );
            final IObservation observation = ZmlFactory.parseXML( linkURLT, "ID_T" ); //$NON-NLS-1$
            final FileWriter writer = new FileWriter( targetFileT );
            NAZMLGenerator.createExt2File( writer, observation, conf.getSimulationStart(), conf.getSimulationEnd(), TimeserieConstants.TYPE_TEMPERATURE, "1.0" ); //$NON-NLS-1$
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
            // final URL linkURLV = m_urlUtils.resolveURL( m_workspace.getContext(), hrefV );
            final URL linkURLV = m_urlUtils.resolveURL( conf.getZMLContext(), hrefV );
            final IObservation observation = ZmlFactory.parseXML( linkURLV, "ID_V" ); //$NON-NLS-1$
            final FileWriter writer = new FileWriter( targetFileV );
            NAZMLGenerator.createExt2File( writer, observation, conf.getSimulationStart(), conf.getSimulationEnd(), TimeserieConstants.TYPE_EVAPORATION, "0.5" ); //$NON-NLS-1$
            IOUtils.closeQuietly( writer );
          }
        }
      }
    }
  }

  private void addDownStream( NetElement downStreamElement )
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

  public List getDownStreamNetElements( )
  {
    return m_downStreamDepends;
  }

  public List getUpStreamNetElements( )
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

    final IRelationType rt = (IRelationType) m_channelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
    final Feature knotu = m_workspace.resolveLink( m_channelFE, rt );
    if( knotu == null )
      System.out.println( "knotU=null" ); //$NON-NLS-1$
    asciiBuffer.getNetBuffer().append( "   " + virtualChannelId ); //$NON-NLS-1$
    asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( knotu ), "i8" ) ); //$NON-NLS-1$
    // asciiBuffer.getNetBuffer().append( ASCIIHelper.toAsciiLine( knotu, m_netAsciiFormat[11] ) );
    asciiBuffer.getNetBuffer().append( ENDKNOTEN );
    asciiBuffer.getNetBuffer().append( " 0\n" ); //$NON-NLS-1$

    asciiBuffer.getChannelBuffer().append( virtualChannelId + "\n" ); //$NON-NLS-1$
    asciiBuffer.getChannelBuffer().append( ChannelManager.VIRTUALCHANNEL + "\n" ); //$NON-NLS-1$
    // m_virtual = true;
  }

  /**
   * writes part 1 of netfile
   */
  public void write( AsciiBuffer asciiBuffer, List<Feature> nodeList )
  {
    final IDManager idManager = m_conf.getIdManager();
    asciiBuffer.addFeatureToWrite( getChannel() );

    final IRelationType rt = (IRelationType) m_channelFE.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
    final Feature knotU = m_workspace.resolveLink( m_channelFE, rt );

    // append channel:
    asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( m_channelFE ), "i8" ) ); //$NON-NLS-1$

    final IFeatureType nodeFT = m_manager.m_conf.getNodeFT();
    final Feature[] features = m_workspace.getFeatures( nodeFT );
    final IRelationType downStreamChannelMemberRT = (IRelationType) nodeFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
    Feature knotO = null;
    for( int i = 0; i < features.length; i++ )
    {
      if( m_channelFE == m_workspace.resolveLink( features[i], downStreamChannelMemberRT ) )
      {
        knotO = features[i];
        continue;
      }
    }
    m_calculated = true;
    // collect related catchments
    final List<Feature> catchmentList = new ArrayList<Feature>();
    final IFeatureType catchemtFT = m_manager.m_conf.getCatchemtFT();
    final Feature[] Cfeatures = m_workspace.getFeatures( catchemtFT );

    final IRelationType entwaesserungsStrangMemberRT = (IRelationType) catchemtFT.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
    for( int i = 0; i < Cfeatures.length; i++ )
    {
      if( m_channelFE == m_workspace.resolveLink( Cfeatures[i], entwaesserungsStrangMemberRT ) )
        catchmentList.add( Cfeatures[i] );
    }

    // append upstream node:
    if( knotO != null )
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( knotO ), "i8" ) ); //$NON-NLS-1$
    else
      asciiBuffer.getNetBuffer().append( ANFANGSKNOTEN );

    // append downstream node:
    asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( knotU ), "i8" ) ); //$NON-NLS-1$

    asciiBuffer.getNetBuffer().append( " " + catchmentList.size() + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    for( Iterator iter = catchmentList.iterator(); iter.hasNext(); )
    {
      Feature catchmentFE = (Feature) iter.next();
      asciiBuffer.addFeatureToWrite( catchmentFE );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( catchmentFE ), "i8" ) ); //$NON-NLS-1$
      asciiBuffer.getNetBuffer().append( "\n" ); //$NON-NLS-1$
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