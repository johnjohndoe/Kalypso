/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Comparator;
import java.util.Formatter;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Converts discretisation model to SMS .2dm file
 * 
 * @author Thomas Jung
 */
public class Gml2SMSConv implements INativeIDProvider, I2DMeshConverter
{
  public static final boolean SUPPORT_MIDSIDE_NODES = false;

  public static final boolean SUPPORT_FLOW_RESISTANCE_CLASSES = false;

  private final IdMap m_roughnessIDProvider;

  private final IdMap m_nodesIDProvider = new IdMap();

  private final IdMap m_elementsIDProvider = new IdMap();

  private final IdMap m_complexElementsIDProvider = new IdMap();

  private final IdMap m_linesIDProvider = new IdMap();

  // TODO probably identical to m_nodesProvider (its key set))
  private final Set<String> m_writtenNodesIDs = new HashSet<>();

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  // TODO: check: calculation?
  public Gml2SMSConv( final IFEDiscretisationModel1d2d discretisationModel1d2d, final IRoughnessClsCollection roughnessModel )
  {
    m_discretisationModel1d2d = discretisationModel1d2d;

    // initialize Roughness IDs
    // TODO: Fishy!
    if( roughnessModel == null )
      m_roughnessIDProvider = null;
    else
    {
      m_roughnessIDProvider = new IdMap( roughnessModel.getRoughnessClasses().size() );
      for( final IRoughnessCls o : roughnessModel.getRoughnessClasses() )
        m_roughnessIDProvider.getOrAdd( o.getId() );
    }
  }

  /**
   * @return <code>0</code>, if feature is <code>null</code> or of unknown type.
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  @Override
  public int getConversionID( final Feature feature )
  {
    if( feature == null ) // TODO: this is probably an error in the data, throw an exception instead?
      return 0;

    final String id = feature.getId();
    if( feature instanceof IFE1D2DNode )
      return m_nodesIDProvider.getOrAdd( id );

    if( feature instanceof IFE1D2DElement || feature instanceof IJunctionElement )
      return m_elementsIDProvider.getOrAdd( id );

    if( feature instanceof IFELine )
      return m_linesIDProvider.getOrAdd( id );

    if( feature instanceof IFE1D2DComplexElement )
      return m_complexElementsIDProvider.getOrAdd( id );

    if( feature instanceof IRoughnessCls )
      return m_roughnessIDProvider.getOrAdd( id );

    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  @Override
  public int getConversionID( final String featureGmlID )
  {
    if( m_nodesIDProvider.contains( featureGmlID ) )
      return m_nodesIDProvider.getOrAdd( featureGmlID );

    if( m_elementsIDProvider.contains( featureGmlID ) )
      return m_elementsIDProvider.getOrAdd( featureGmlID );

    if( m_linesIDProvider.contains( featureGmlID ) )
      return m_linesIDProvider.getOrAdd( featureGmlID );

    if( m_complexElementsIDProvider.contains( featureGmlID ) )
      return m_complexElementsIDProvider.getOrAdd( featureGmlID );

    if( m_roughnessIDProvider.contains( featureGmlID ) )
      return m_roughnessIDProvider.getOrAdd( featureGmlID );

    return 0;
  }

  @Override
  public void writeMesh( final File outputFile ) throws CoreException, IOException
  {
    Formatter formatter = null;
    try
    {
      formatter = new Formatter( outputFile, Charset.defaultCharset().name(), Locale.US );
      writeRMA10sModel( formatter );
      FormatterUtils.checkIoException( formatter );
    }
    finally
    {
      if( formatter != null )
      {
        // REMARK: do not check io-exception here, else other exception would be hidden by this on
        formatter.close();
      }
    }
  }

  private void writeRMA10sModel( final Formatter formatter ) throws CoreException, IOException
  {
    writeHeaderLine( formatter );

    final IFE1D2DElement[] elements = m_discretisationModel1d2d.getElements();
    writeElementsAndNodes( formatter, elements );
  }

  /**
   * writes the header for .2dm file <br>
   * <br>
   * <b>Format specification:</b><br>
   * Card Type: MESH2D <br>
   * Description: Identifies the file as a 2d mesh file. Must be the first line of the file. <br>
   * Required: YES
   */
  private void writeHeaderLine( final Formatter formatter )
  {
    formatter.format( "MESH2D%n" ); //$NON-NLS-1$
  }

  private void writeNodes( final Formatter formatter, final IFE1D2DNode[] nodes ) throws CoreException, IOException
  {
    for( final IFE1D2DNode node : nodes )
    {
      // TODO: only write nodes, which are within the requested calculation unit!

      if( m_writtenNodesIDs.contains( node.getId() ) )
        continue;

      checkElevation( node );

      m_writtenNodesIDs.add( node.getId() );

      /* The node itself */
      final int nodeID = getConversionID( node );
      final GM_Point point = node.getPoint();

      if( node.getAdjacentElements()[0] instanceof IElement1D )
      {
        // not handled
      }
      formatNode( formatter, nodeID, point );
      FormatterUtils.checkIoException( formatter );
    }
  }

  private void checkElevation( final IFE1D2DNode node ) throws CoreException
  {
    // check if node elevation is assigned
    double z = Double.NaN;
    try
    {
      z = node.getPoint().getZ();
    }
    catch( final ArrayIndexOutOfBoundsException e )
    {
      // could happen that the node only has x and y coordinates
      // ignore now, case will be handled soon, z = Double.NaN;
    }
    if( Double.isNaN( z ) )
    {
      final double x = node.getPoint().getX();
      final double y = node.getPoint().getY();

      final String msg = String.format( "Keine H�hendaten: [%.3f, %.3f]", x, y ); //$NON-NLS-1$
      // TODO: georefed error msg
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
    }
  }

  private void formatNode( final Formatter formatter, final int nodeID, final GM_Point point )
  {
    /* Now really write the nodes */
    final double x = point.getX();
    final double y = point.getY();
    final double z = point.getZ();

    /*
     * Card Type ND Description: Defines the ID and location for each node of the mesh. Required: NO Format: ND id x y z
     * Sample: ND 1 7.75e+005 1.10e+005 5.00e-001 id (The ID of the node), x,y,z (the x, y, and z coordinates of the
     * point).
     */

    formatter.format( "ND%10d%20.7f%20.7f%20.7f%n", nodeID, x, y, z ); //$NON-NLS-1$
  }

  /**
   * write elements nodes and edges in a way which avoids the filtering of edges and nodes
   */
  private void writeElementsAndNodes( final Formatter formatter, final IFE1D2DElement[] elementsInBBox ) throws CoreException, IOException
  {
    final Set<IFE1D2DEdge> edgeSet = new HashSet<>( elementsInBBox.length * 2 );

    if( elementsInBBox.length == 0 )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2SMSConv.2" ) ) ); //$NON-NLS-1$

    for( final IFE1D2DElement element : elementsInBBox )
    {
      if( element instanceof IElement1D )
        continue;

      final int id = getConversionID( element );

      if( element instanceof IElement1D )
      {
        // not handled
      }
      else if( element instanceof IPolyElement )
      {
        for( final IFE1D2DEdge edge : ((IPolyElement)element).getEdges() )
        {
          edgeSet.add( edge );
        }

        final int roughnessID = m_roughnessIDProvider == null ? 0 : getRoughnessID( element );
        final IFE1D2DNode[] nodes = element.getNodes();

        if( nodes.length == 4 )
        {
          writeTriangularElement( formatter, id, roughnessID, nodes );
        }
        else if( nodes.length == 5 )
        {
          writeQuadrangularElement( formatter, id, roughnessID, nodes );
        }
      }
    }
    FormatterUtils.checkIoException( formatter );

    // write edge set nodes

    final Comparator<IFE1D2DNode> comparator = new Comparator<IFE1D2DNode>()
    {
      @Override
      public int compare( final IFE1D2DNode o1, final IFE1D2DNode o2 )
      {
        final Integer id1 = getConversionID( o1 );
        final Integer id2 = getConversionID( o2 );
        return id1.compareTo( id2 );
      }
    };

    final SortedSet<IFE1D2DNode> nodeSet = new TreeSet<>( comparator );

    for( final IFE1D2DEdge edge : edgeSet )
    {
      final IFE1D2DNode[] nodes = edge.getNodes();
      for( final IFE1D2DNode node : nodes )
      {
        nodeSet.add( node );
      }
    }
    final IFE1D2DNode[] nodes = nodeSet.toArray( new IFE1D2DNode[nodeSet.size()] );
    writeNodes( formatter, nodes );
  }

  /**
   * writes elements with 4 nodes. <br>
   * <br>
   * <b>Format specification:</b><br>
   * Card Type E4Q <br>
   * Description: Identifies a 4-noded quadrilateral element. <br>
   * Required: NO <br>
   * Format: E4Q id n1 n2 n3 n4 matid <br>
   * Sample: E4Q 1 1 2 3 4 1 (Field, Variable, Value, Description) <br>
   * id: The ID of the element. <br>
   * n1-n4: The ID's of nodes in the element.<br>
   * matid: The ID of the material assigned to the element.
   */
  private void writeQuadrangularElement( final Formatter formatter, final int id, final int roughnessID, final IFE1D2DNode[] nodes )
  {
    // TODO: check orientation

    final int nodeID1 = getConversionID( nodes[0] );
    final int nodeID2 = getConversionID( nodes[1] );
    final int nodeID3 = getConversionID( nodes[2] );
    final int nodeID4 = getConversionID( nodes[3] );

    formatter.format( "E4Q%10d%10d%10d%10d%10d%10d%n", id, nodeID1, nodeID2, nodeID3, nodeID4, roughnessID ); //$NON-NLS-1$
  }

  /**
   * writes elements with 3 nodes. <br>
   * <br>
   * <b>Format specification:</b><br>
   * Card Type E3T<br>
   * Description: Identifies a 3-noded triangular element. <br>
   * Required: NO <br>
   * Format: E3T id n1 n2 n3 matid <br>
   * Sample: E3T 1 1 2 3 1 (Field, Variable, Value, Description) <br>
   * id: The ID of the element.<br>
   * n1-n3: The ID's of nodes in the element.<br>
   * matid: The ID of the material assigned to the element.
   */
  private void writeTriangularElement( final Formatter formatter, final int id, final int roughnessID, final IFE1D2DNode[] nodes )
  {

    // TODO: check orientation

    final int nodeID1 = getConversionID( nodes[0] );
    final int nodeID2 = getConversionID( nodes[1] );
    final int nodeID3 = getConversionID( nodes[2] );

    formatter.format( "E3T%10d%10d%10d%10d%10d%n", id, nodeID1, nodeID2, nodeID3, roughnessID ); //$NON-NLS-1$
  }

  private int getRoughnessID( final IFE1D2DElement element ) throws CoreException
  {
    final String roughnessClsID = element.getRoughnessClsID();
    if( roughnessClsID != null && roughnessClsID.length() > 0 )
      return m_roughnessIDProvider.getOrAdd( roughnessClsID );

    // TODO: georefed, core exception!
    final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.Gml2SMSConv.5", roughnessClsID, element ); //$NON-NLS-1$
    // TODO: use default zone instead
    throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg ) );
  }

  @Override
  public boolean supportFlowResistanceClasses( )
  {
    return SUPPORT_FLOW_RESISTANCE_CLASSES;
  }

  @Override
  public boolean supportMidsideNodes( )
  {
    // TODO Auto-generated method stub
    return SUPPORT_MIDSIDE_NODES;
  }
}