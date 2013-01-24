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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.model.Branching;
import org.kalypso.model.hydrology.binding.model.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.KontEntnahme;
import org.kalypso.model.hydrology.binding.model.KontZufluss;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.Ueberlauf;
import org.kalypso.model.hydrology.binding.model.Verzweigung;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.preprocessing.RelevantNetElements;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;
import org.kalypso.model.hydrology.internal.preprocessing.timeseries.GrapWriter;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Writes the collected net elements etc. into the .ntz file.
 * 
 * @author doemming
 */
public class NetFileWriter extends AbstractCoreFileWriter
{
  private static final class ZuflussBean
  {
    public final StringBuffer m_specialBuffer = new StringBuffer();

    public final int m_izug;

    public final int m_iabg;

    public final int m_iueb;

    public final int m_izuf;

    public final int m_ivzwg;

    public final Number m_value;

    public final Node m_branchNode;

    public ZuflussBean( final int izug, final int iabg, final int iueb, final int izuf, final int ivzwg, final double value, final Node branchNode )
    {
      m_izug = izug;
      m_iabg = iabg;
      m_iueb = iueb;
      m_izuf = izuf;
      m_ivzwg = ivzwg;
      m_value = value;
      m_branchNode = branchNode;
    }
  }

  private final UrlUtilities m_urlUtilities = new UrlUtilities();

  private final RelevantNetElements m_relevantElements;

  private final IDManager m_idManager;

  private final NAControl m_metaControl;

  private final NaAsciiDirs m_asciiDirs;

  private final URL m_zmlContext;

  public NetFileWriter( final NaAsciiDirs asciiDirs, final RelevantNetElements relevantElements, final IDManager idManager, final URL zmlContext, final NAControl metaControl, final Logger logger )
  {
    super( logger );

    m_asciiDirs = asciiDirs;

    m_relevantElements = relevantElements;
    m_idManager = idManager;
    m_zmlContext = zmlContext;
    m_metaControl = metaControl;
  }

  /**
   * @see org.kalypso.model.hydrology.internal.preprocessing.writer.AbstractCoreFileWriter#writeContent(java.io.PrintWriter)
   */
  @Override
  protected void writeContent( final PrintWriter writer ) throws Exception
  {
    final NetElement[] channels = m_relevantElements.getChannels();
    for( final NetElement netElement : channels )
      netElement.write( writer );

    final Entry<NetElement, Integer>[] rootChannels = m_relevantElements.getRootChannels();
    for( final Entry<NetElement, Integer> entry : rootChannels )
    {
      final NetElement netElement = entry.getKey();
      final Integer virtualChannelId = entry.getValue();
      netElement.writeRootChannel( writer, virtualChannelId );
    }

    final Node[] nodeCollector = m_relevantElements.getNodes();
    writer.append( "99999\n" ); //$NON-NLS-1$
    appendNodeList( nodeCollector, writer );
    writer.append( "99999\n" ); //$NON-NLS-1$
  }

  private void appendNodeList( final Node[] nodes, final PrintWriter netBuffer ) throws Exception, Exception
  {
    // FIXME: theses nodes do not contain the branching nodes

    final Map<Node, ZuflussBean> nodeInfos = new LinkedHashMap<Node, ZuflussBean>();

    /* First collect info and potentially add branching nodes */
    for( final Node node : nodes )
    {
      // QQ have the priority over "Verzweigung", and all of them are mutually exclusive so this is safe
      final ZuflussBean qqRelation = getQQRelationZuflussBean( node );
      final ZuflussBean zuflussBean = qqRelation != null ? qqRelation : appendZuflussStuff( node );
      nodeInfos.put( node, zuflussBean );

      if( zuflussBean.m_branchNode != null )
      {
        /* Do not overwrite existing info */
        if( !nodeInfos.containsKey( zuflussBean.m_branchNode ) )
          nodeInfos.put( zuflussBean.m_branchNode, new ZuflussBean( 0, 0, 0, 0, 0, Double.NaN, null ) );
      }
    }

    /* Write thes infos to file */
    final Set<Entry<Node, ZuflussBean>> entrySet = nodeInfos.entrySet();
    for( final Entry<Node, ZuflussBean> entry : entrySet )
    {
      final Node node = entry.getKey();
      final ZuflussBean zuflussBean = entry.getValue();

      final int nodeID = m_idManager.getAsciiID( node );

      netBuffer.append( FortranFormatHelper.printf( nodeID, "i5" ) ); //$NON-NLS-1$

      netBuffer.append( String.format( "%5d", zuflussBean.m_izug ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_iabg ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_iueb ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_izuf ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_ivzwg ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      netBuffer.append( zuflussBean.m_specialBuffer.toString() );

      /* TODO: we should also consider the additional nodes by QQ rleations; but as QQ rleations do not work..... */
    }

    // ENDKNOTEN
    netBuffer.append( " 9001    0    0    0    0    0\n" ); //$NON-NLS-1$
    netBuffer.append( "10000    0    0    0    0    0\n" ); //$NON-NLS-1$
  }

  private ZuflussBean getQQRelationZuflussBean( final Node node ) throws SensorException
  {
    final Node relatedNode = node.getQQRelatedNode();
    if( relatedNode == null )
      return null;
    final IObservation observation = (IObservation) node.getProperty( NaModelConstants.NODE_QQRELATION_PROP );
    if( observation == null )
      return null;
    final StringBuffer buffer = new StringBuffer();
    final IAxis[] axisList = observation.getAxes();
    final IAxis q1Axis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF );
    final IAxis q2Axis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF_RHB );
    final ITupleModel values = observation.getValues( null );
    final int count = values.size();
    if( count < 1 )
      return null;
    buffer.append( String.format( "%4d %6s\n", count, m_idManager.getAsciiID( relatedNode ) ) ); //$NON-NLS-1$
    for( int row = 0; row < count; row++ )
    {
      final double q1 = (Double) values.get( row, q1Axis );
      final double q2 = (Double) values.get( row, q2Axis );
      buffer.append( String.format( Locale.ENGLISH, "%8.3f %8.3f\n", q1, q2 ) ); //$NON-NLS-1$
    }
    final ZuflussBean bean = new ZuflussBean( 0, 0, 0, 0, 2, count, relatedNode );
    bean.m_specialBuffer.append( buffer );
    return bean;
  }

  private ZuflussBean appendZuflussStuff( final Node node ) throws Exception
  {
    // TODO: like this, only one branching can be used at the same time. Also, braching and zufluss cannot appear at
    // the same time. Is both intented? Isn't kalypso-na able to do more?
    final Branching branching = node.getBranching();
    if( branching != null )
      return appendBranching( branching );

    final TimeseriesLinkType zuflussLink = node.getZuflussLink();
    if( zuflussLink != null )
      return appendZuflussLink( node, zuflussLink );

    return new ZuflussBean( 0, 0, 0, 0, 0, Double.NaN, null );
  }

  private ZuflussBean appendZuflussLink( final Node node, final TimeseriesLinkType zuflussLink ) throws Exception
  {
    final ZuflussBean bean = new ZuflussBean( 0, 0, 0, 5, 0, Double.NaN, null );

    final String zuflussFileName = getZuflussEingabeDateiString( node );
    final File targetFile = new File( m_asciiDirs.zuflussDir, zuflussFileName );

    final String zuflussFile = ZmlURL.getIdentifierPart( zuflussLink.getHref() );
    final URL linkURL = m_urlUtilities.resolveURL( m_zmlContext, zuflussFile );
    if( !targetFile.exists() )
    {
      final StringBuffer writer = new StringBuffer();
      final IObservation observation = ZmlFactory.parseXML( linkURL ); //$NON-NLS-1$

      final Boolean isSynteticZufluss = node.isSynteticZufluss();

      if( isSynteticZufluss != null && isSynteticZufluss )
      {
        final Integer minutesOfTimestep = m_metaControl.getMinutesOfTimestep();

        if( m_metaControl.isUsePrecipitationForm() )
        {
          final ITupleModel values = observation.getValues( null );
          final IAxis[] axis = observation.getAxes();
          final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, ITimeseriesConstants.TYPE_DATE );
          final long simulationStartDateMillis = ((Date) values.get( 0, dateAxis )).getTime();
          final long simulationEndDateMillis = ((Date) values.get( values.size() - 1, dateAxis )).getTime();
          final Date simulationStartDate = new Date( 100, 0, 1 );
          final Date simulationEndDate = new Date( simulationStartDate.getTime() + simulationEndDateMillis - simulationStartDateMillis );

          final GrapWriter grapWriter = new GrapWriter( ITimeseriesConstants.TYPE_RUNOFF, observation );
          grapWriter.writeSyntheticFile( writer, simulationStartDate, simulationEndDate, minutesOfTimestep );
        }
        else
        {
          final Date simulationStart = m_metaControl.getSimulationStart();
          final Date simulationEnd = m_metaControl.getSimulationEnd();
          final GrapWriter grapWriter = new GrapWriter( ITimeseriesConstants.TYPE_RUNOFF, observation );
          grapWriter.writeSyntheticFile( writer, simulationStart, simulationEnd, minutesOfTimestep );
        }
      }
      else
      {
        final GrapWriter grapWriter = new GrapWriter( ITimeseriesConstants.TYPE_RUNOFF, observation );
        grapWriter.write( writer );
      }

      FileUtils.writeStringToFile( targetFile, writer.toString() );
    }

    bean.m_specialBuffer.append( "    1234\n" ); // dummyLine //$NON-NLS-1$
    bean.m_specialBuffer.append( ".." + File.separator + "zufluss" + File.separator + zuflussFileName + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    return bean;
  }

  private ZuflussBean appendBranching( final Branching branching )
  {
    final ZuflussBean bean = getZuflussBean( branching );
    if( bean.m_value instanceof Double )
      bean.m_specialBuffer.append( String.format( Locale.US, "%10.3f", bean.m_value ) ); //$NON-NLS-1$
    else if( bean.m_value instanceof Integer )
      bean.m_specialBuffer.append( String.format( "%4d", bean.m_value ) ); //$NON-NLS-1$
    // else throw an exception...

    if( bean.m_branchNode != null )
    {
      final int branchNodeID = m_idManager.getAsciiID( bean.m_branchNode );
      bean.m_specialBuffer.append( FortranFormatHelper.printf( branchNodeID, "i8" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    return bean;
  }

  private ZuflussBean getZuflussBean( final Branching branching )
  {
    final Node branchNode;
    if( branching instanceof BranchingWithNode )
      branchNode = ((BranchingWithNode) branching).getNode();
    else
      branchNode = null;

    if( branching instanceof KontZufluss )
    {
      final double qzug = ((KontZufluss) branching).getQZug();
      return new ZuflussBean( 1, 0, 0, 0, 0, qzug, branchNode );
    }

    if( branching instanceof Verzweigung )
    {
      final double zproz = ((Verzweigung) branching).getZProz();
      return new ZuflussBean( 0, 0, 0, 0, 1, zproz, branchNode );
    }

    if( branching instanceof KontEntnahme )
    {
      final double qabg = ((KontEntnahme) branching).getQAbg();
      return new ZuflussBean( 0, 1, 0, 0, 0, qabg, branchNode );
    }

    if( branching instanceof Ueberlauf )
    {
      final double queb = ((Ueberlauf) branching).getQUeb();
      return new ZuflussBean( 0, 0, 1, 0, 0, queb, branchNode );
    }

    throw new IllegalArgumentException( "Illegal branching type: " + branching.getClass() );
  }

  private String getZuflussEingabeDateiString( final Feature nodeFE )
  {
    final int asciiID = m_idManager.getAsciiID( nodeFE );
    return "Z_" + Integer.toString( asciiID ).trim() + ".zufluss"; //$NON-NLS-1$ //$NON-NLS-2$
  }

}