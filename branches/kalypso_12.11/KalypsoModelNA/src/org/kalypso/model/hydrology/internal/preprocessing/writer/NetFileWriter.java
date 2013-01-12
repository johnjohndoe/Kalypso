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
import java.io.IOException;
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
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.KontEntnahme;
import org.kalypso.model.hydrology.binding.model.KontZufluss;
import org.kalypso.model.hydrology.binding.model.Ueberlauf;
import org.kalypso.model.hydrology.binding.model.nodes.Branching;
import org.kalypso.model.hydrology.binding.model.nodes.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.binding.model.nodes.Verzweigung;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.NetElement;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.RelevantNetElements;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Writes the collected net elements etc. into the .ntz file.
 * 
 * @author doemming
 */
class NetFileWriter extends AbstractCoreFileWriter
{
  private final IStatusCollector m_log = new StatusCollector( ModelNA.PLUGIN_ID );

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

  @Override
  protected void writeContent( final PrintWriter writer ) throws IOException, NAPreprocessorException
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

    try
    {
      final Node[] nodeCollector = m_relevantElements.getNodes();
      writer.append( "99999\n" ); //$NON-NLS-1$
      appendNodeList( nodeCollector, writer );
      writer.append( "99999\n" ); //$NON-NLS-1$
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( Messages.getString( "NetFileWriter_0" ), e ); //$NON-NLS-1$
    }
  }

  private void appendNodeList( final Node[] nodes, final PrintWriter netBuffer ) throws IOException, SensorException
  {
    // FIXME: theses nodes do not contain the branching nodes
    final Map<Node, ZuflussBean> nodeInfos = new LinkedHashMap<>();

    /* First collect info and potentially add branching nodes */
    for( final Node node : nodes )
    {
      final ZuflussBean zuflussBean = createZuflussBean( node );

      // FIXME: strange we map the source-node here...
      nodeInfos.put( node, zuflussBean );

      final Node branchingNode = zuflussBean.getBranchingNode();
      if( branchingNode != null )
      {
        // FIXME: ... but check for the target node here; this can't be right, can it?
        // FIXME: comment is not correct
        /* Do not overwrite existing info */

        // FIXME: in reality, we make sure a node that is the target of a branching, gets a default bean; but this happens anyways, so what? -> if the target itself has a branching, it might get
        // removed, if the nodes are in the correct order...
        if( !nodeInfos.containsKey( branchingNode ) )
          nodeInfos.put( branchingNode, new ZuflussBean( 0, 0, 0, 0, 0, null, StringUtils.EMPTY ) );
      }
    }

    /* Write thes infos to file */
    final Set<Entry<Node, ZuflussBean>> entrySet = nodeInfos.entrySet();
    for( final Entry<Node, ZuflussBean> entry : entrySet )
    {
      final Node node = entry.getKey();
      final ZuflussBean zuflussBean = entry.getValue();

      final int nodeID = m_idManager.getAsciiID( node );

      netBuffer.format( "%5d", nodeID ); //$NON-NLS-1$

      netBuffer.format( "%5d", zuflussBean.getIzug() ); //$NON-NLS-1$
      netBuffer.format( "%5d", zuflussBean.getIabg() ); //$NON-NLS-1$
      netBuffer.format( "%5d", zuflussBean.getIueb() ); //$NON-NLS-1$
      netBuffer.format( "%5d", zuflussBean.getIzuf() ); //$NON-NLS-1$
      netBuffer.format( "%5d\n", zuflussBean.getIvzwg() ); //$NON-NLS-1$ //$NON-NLS-2$

      netBuffer.append( zuflussBean.getArgument() );

      // FIXME: what additional nodes?
      /* TODO: we should also consider the additional nodes by QQ rleations; but as QQ rleations do not work..... */
    }

    // ENDKNOTEN
    netBuffer.append( " 9001    0    0    0    0    0\n" ); //$NON-NLS-1$
    netBuffer.append( "10000    0    0    0    0    0\n" ); //$NON-NLS-1$
  }

  private ZuflussBean createZuflussBean( final Node node ) throws SensorException, IOException
  {
    /* first create all possible beans, so we can check if we have multiple branchings */
    final ZuflussBean qqRelationBean = createQQRelationZuflussBean( node );
    final ZuflussBean branchingBean = createBranchingZuflussBean( node );
    final ZuflussBean zuflussLinkBean = createZuflussLinkZuflussBean( node );

    /* validation */
    final int count = countZuflussBeans( qqRelationBean, branchingBean, zuflussLinkBean );
    if( count > 1 )
      m_log.add( IStatus.WARNING, Messages.getString( "NetFileWriter.0" ), null, node.getName() ); //$NON-NLS-1$

    // QQ has the priority over "Verzweigung", and all of them are mutually exclusive so this is safe
    if( qqRelationBean != null )
      return qqRelationBean;

    // REMARK: only one branching can be used at the same time. Also, braching and zufluss cannot appear at
    // the same time. This is intended.
    if( branchingBean != null )
      return branchingBean;

    if( zuflussLinkBean != null )
      return zuflussLinkBean;

    return new ZuflussBean( 0, 0, 0, 0, 0, null, StringUtils.EMPTY );
  }

  private int countZuflussBeans( final ZuflussBean... beans )
  {
    int count = 0;
    for( final ZuflussBean bean : beans )
    {
      if( bean != null )
        count++;
    }

    return count;
  }

  private ZuflussBean createQQRelationZuflussBean( final Node node ) throws SensorException
  {
    final Node relatedNode = node.getQQRelatedNode();
    if( relatedNode == null )
      return null;

    final IObservation observation = (IObservation)node.getProperty( INode.PROPERTY_QQ_RELATION );
    if( observation == null )
      return null;

    /* serialize the qq relation */
    final StringBuilder buffer = new StringBuilder();
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
      final double q1 = (Double)values.get( row, q1Axis );
      final double q2 = (Double)values.get( row, q2Axis );
      buffer.append( String.format( Locale.ENGLISH, "%8.3f %8.3f\n", q1, q2 ) ); //$NON-NLS-1$
    }

    final String value = buffer.toString();

    return new ZuflussBean( 0, 0, 0, 0, 2, relatedNode, value );
  }

  private ZuflussBean createZuflussLinkZuflussBean( final Node node ) throws SensorException, IOException
  {
    final ZmlLink zuflussLink = node.getZuflussLink();
    if( !zuflussLink.isLinkSet() )
      return null;

    /* write zufluss file */
    final String zuflussFileName = getZuflussEingabeDateiString( node );
    final File targetFile = new File( m_asciiDirs.zuflussDir, zuflussFileName );

    final String zuflussFile = ZmlURL.getIdentifierPart( zuflussLink.getHref() );
    final URL linkURL = m_urlUtilities.resolveURL( m_zmlContext, zuflussFile );
    if( !targetFile.exists() )
    {
      final StringBuilder writer = new StringBuilder();
      final IObservation observation = ZmlFactory.parseXML( linkURL ); //$NON-NLS-1$

      // FIXME: this functionality is currently deactivated, because it is wrongly implemented.
      final boolean isSynteticZufluss = false;
      // node.isSynteticZufluss();
      if( isSynteticZufluss )
      {
        final Integer minutesOfTimestep = m_metaControl.getMinutesOfTimestep();

        if( m_metaControl.isUsePrecipitationForm() )
        {
          final ITupleModel values = observation.getValues( null );
          final IAxis[] axis = observation.getAxes();
          final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, ITimeseriesConstants.TYPE_DATE );
          final long simulationStartDateMillis = ((Date)values.get( 0, dateAxis )).getTime();
          final long simulationEndDateMillis = ((Date)values.get( values.size() - 1, dateAxis )).getTime();

          // FIXME: this will not work correctly with timezones

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

    /* create arguent */
    final StringBuilder buffer = new StringBuilder();
    buffer.append( "    1234\n" ); // dummyLine //$NON-NLS-1$
    buffer.append( ".." ).append( File.separator ).append( "zufluss" ).append( File.separator ).append( zuflussFileName ).append( "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    final String argument = buffer.toString();

    /* create bean entry */
    return new ZuflussBean( 0, 0, 0, 5, 0, null, argument );
  }

  private ZuflussBean createBranchingZuflussBean( final Node node )
  {
    final Branching branching = node.getBranching();
    if( branching == null )
      return null;

    final Node branchNode;
    if( branching instanceof BranchingWithNode )
      branchNode = ((BranchingWithNode)branching).getNode();
    else
      branchNode = null;

    String branchIdArgument = StringUtils.EMPTY;
    if( branchNode != null )
    {
      final int branchNodeID = m_idManager.getAsciiID( branchNode );
      branchIdArgument = String.format( " %7d\n", branchNodeID ); //$NON-NLS-1$
    }

    if( branching instanceof KontZufluss )
    {
      final double qzug = ((KontZufluss)branching).getQZug();
      final String argument = String.format( Locale.US, "%10.3f", qzug ); //$NON-NLS-1$
      return new ZuflussBean( 1, 0, 0, 0, 0, branchNode, argument );
    }

    if( branching instanceof Verzweigung )
    {
      final double zproz = ((Verzweigung)branching).getZProz();
      final String argument = String.format( Locale.US, "%10.3f", zproz ); //$NON-NLS-1$
      return new ZuflussBean( 0, 0, 0, 0, 1, branchNode, argument + branchIdArgument );
    }

    if( branching instanceof KontEntnahme )
    {
      final double qabg = ((KontEntnahme)branching).getQAbg();
      final String argument = String.format( Locale.US, "%10.3f", qabg ); //$NON-NLS-1$
      return new ZuflussBean( 0, 1, 0, 0, 0, branchNode, argument + branchIdArgument );
    }

    if( branching instanceof Ueberlauf )
    {
      final double queb = ((Ueberlauf)branching).getQUeb();
      final String argument = String.format( Locale.US, "%10.3f", queb ); //$NON-NLS-1$
      return new ZuflussBean( 0, 0, 1, 0, 0, branchNode, argument + branchIdArgument );
    }

    throw new IllegalArgumentException( "Illegal branching type: " + branching.getClass() ); //$NON-NLS-1$
  }

  private String getZuflussEingabeDateiString( final Feature nodeFE )
  {
    final int asciiID = m_idManager.getAsciiID( nodeFE );
    return "Z_" + Integer.toString( asciiID ).trim() + ".zufluss"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public IStatus getStatus( )
  {
    return m_log.asMultiStatusOrOK( Messages.getString( "NetFileWriter.2" ) ); //$NON-NLS-1$
  }
}