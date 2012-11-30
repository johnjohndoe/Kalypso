/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.ogc.gml.serialize.Gml2HmoConverter;
import org.kalypso.ogc.gml.serialize.HMOSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Converts discretisation model to .hmo file
 * 
 * @author felipe maximino
 */
public class GmlMesh2HmoConverter extends Gml2HmoConverter implements I2DMeshConverter, INativeIDProvider
{
  public static final boolean SUPPORT_MIDSIDE_NODES = false;

  public static final boolean SUPPORT_FLOW_RESISTANCE_CLASSES = false;

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final IdMap m_nodesIDProvider = new IdMap();

  public GmlMesh2HmoConverter( final IFEDiscretisationModel1d2d discretisationModel1d2d )
  {
    m_discretisationModel1d2d = discretisationModel1d2d;
  }

  public void writeElements( final HMOSerializer hmoSerializer, final IFE1D2DElement[] elementsInBBox ) throws CoreException
  {
    final List<IFE1D2DNode[]> triangularElements = new ArrayList<>();

    if( elementsInBBox.length == 0 )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.GmlMesh2HmoConverter.2" ) ) ); //$NON-NLS-1$

    for( final IFE1D2DElement element : elementsInBBox )
    {
      if( element instanceof IPolyElement )
      {
        final IFE1D2DNode[] nodes = element.getNodes();
        writeNodes( hmoSerializer, nodes );

        if( nodes.length == 4 )
        {
          triangularElements.add( nodes );
        }
        else if( nodes.length == 5 )
        {
          tranformToTriangularElement( nodes, triangularElements );
        }
      }
    }

    writeTriangles( hmoSerializer, triangularElements );
  }

  /* splits quadrangular element in 2 triangular elements */
  private void tranformToTriangularElement( final IFE1D2DNode[] nodes, final List<IFE1D2DNode[]> triangularElements )
  {
    triangularElements.add( new IFE1D2DNode[] { nodes[0], nodes[1], nodes[2] } );
    triangularElements.add( new IFE1D2DNode[] { nodes[0], nodes[2], nodes[3] } );
  }

  private void writeTriangles( final HMOSerializer hmoSerializer, final List<IFE1D2DNode[]> triangularElements )
  {
    int count = 1;
    for( final IFE1D2DNode[] triangle : triangularElements )
    {
      final int nodeID1 = getConversionID( triangle[0] );
      final int nodeID2 = getConversionID( triangle[1] );
      final int nodeID3 = getConversionID( triangle[2] );
      hmoSerializer.formatTriangle( count++, nodeID1, nodeID2, nodeID3 );
    }
  }

  private void writeNodes( final HMOSerializer hmoSerializer, final IFE1D2DNode[] nodes )
  {
    for( final IFE1D2DNode node : nodes )
    {
      if( !m_nodesIDProvider.contains( node.getId() ) )
      {
        final int id = getConversionID( node );
        final GM_Point point = node.getPoint();

        hmoSerializer.formatPoint( id, point.getX(), point.getY(), point.getZ() );
      }
    }
  }

  @Override
  public void writeHmo( final File file ) throws CoreException, IOException
  {
    final HMOSerializer hmoSerializer = new HMOSerializer( file );
    final IFE1D2DElement[] elements = m_discretisationModel1d2d.getElements();
    writeElements( hmoSerializer, elements );
    hmoSerializer.finish();
  }

  @Override
  public void writeMesh( final File file ) throws CoreException, IOException
  {
    writeHmo( file );
  }

  @Override
  public int getConversionID( final Feature feature )
  {
    if( feature == null ) // TODO: this is probably an error in the data, throw an exception instead?
      return 0;

    final String id = feature.getId();
    if( feature instanceof IFE1D2DNode )
      return m_nodesIDProvider.getOrAdd( id );

    return 0;
  }

  @Override
  public int getConversionID( final String featureGmlID )
  {
    if( m_nodesIDProvider.contains( featureGmlID ) )
      return m_nodesIDProvider.getOrAdd( featureGmlID );

    return 0;
  }

  @Override
  public boolean supportFlowResistanceClasses( )
  {
    return SUPPORT_FLOW_RESISTANCE_CLASSES;
  }

  @Override
  public boolean supportMidsideNodes( )
  {
    return SUPPORT_MIDSIDE_NODES;
  }
}