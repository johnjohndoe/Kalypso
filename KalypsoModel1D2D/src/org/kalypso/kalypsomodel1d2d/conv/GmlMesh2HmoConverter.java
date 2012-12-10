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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.ogc.gml.serialize.Gml2HmoConverter;
import org.kalypso.ogc.gml.serialize.HMOSerializer;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Converts discretisation model to .hmo file
 * 
 * @author felipe maximino
 * 
 */
public class GmlMesh2HmoConverter extends Gml2HmoConverter implements I2DMeshConverter, INativeIDProvider
{
  public static final boolean SUPPORT_MIDSIDE_NODES = false;

  public static final boolean SUPPORT_FLOW_RESISTANCE_CLASSES = false;

  private final IFEDiscretisationModel1d2d m_discretisationModel1d2d;

  private final IdMap m_nodesIDProvider = new IdMap();

  HMOSerializer hmoSerializer;

  public GmlMesh2HmoConverter( final IFEDiscretisationModel1d2d discretisationModel1d2d )
  {
    m_discretisationModel1d2d = discretisationModel1d2d;
  }

  @SuppressWarnings("unchecked")
  public void writeElements( IFeatureWrapperCollection<IFE1D2DElement> elements ) throws CoreException
  {
    final List<IFE1D2DElement> elementsInBBox = elements;
    final List<List<IFE1D2DNode>> triangularElements = new ArrayList<List<IFE1D2DNode>>();

    if( elementsInBBox.size() == 0 )
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.GmlMesh2HmoConverter.2" ), null ) ); //$NON-NLS-1$

    for( final IFE1D2DElement element : elementsInBBox )
    {
      if( element instanceof IPolyElement )
      {
        final List<IFE1D2DNode> nodes = element.getNodes();
        writeNodes( nodes );

        if( nodes.size() == 4 )
        {
          triangularElements.add( nodes );
        }
        else if( nodes.size() == 5 )
        {
          tranformToTriangularElement( nodes, triangularElements );
        }
      }
    }

    writeTriangles( triangularElements );
  }

  /* splits quadrangular element in 2 triangular elements */
  private void tranformToTriangularElement( final List<IFE1D2DNode> nodes, List<List<IFE1D2DNode>> triangularElements )
  {
    List<IFE1D2DNode> newTriangle = new ArrayList<IFE1D2DNode>();
    newTriangle.add( nodes.get( 0 ) );
    newTriangle.add( nodes.get( 1 ) );
    newTriangle.add( nodes.get( 2 ) );

    triangularElements.add( newTriangle );

    List<IFE1D2DNode> anotherNewTriangle = new ArrayList<IFE1D2DNode>();

    anotherNewTriangle.add( nodes.get( 0 ) );
    anotherNewTriangle.add( nodes.get( 2 ) );
    anotherNewTriangle.add( nodes.get( 3 ) );

    triangularElements.add( anotherNewTriangle );
  }

  private void writeTriangles( final List<List<IFE1D2DNode>> triangularElements )
  {
    int count = 1;
    for( final List<IFE1D2DNode> triangle : triangularElements )
    {
      final int nodeID1 = getConversionID( triangle.get( 0 ) );
      final int nodeID2 = getConversionID( triangle.get( 1 ) );
      final int nodeID3 = getConversionID( triangle.get( 2 ) );
      hmoSerializer.formatTriangle( count++, nodeID1, nodeID2, nodeID3 );
    }

  }

  private void writeNodes( final List<IFE1D2DNode> nodes )
  {
    for( final IFE1D2DNode node : nodes )
    {
      if( !m_nodesIDProvider.contains( node.getGmlID() ) )
      {
        final int id = getConversionID( node );
        final GM_Point point = node.getPoint();

        hmoSerializer.formatPoint( id, point.getX(), point.getY(), point.getZ() );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.serialize.Gml2HmoConverter#writeHmo(java.io.File)
   */
  @Override
  public void writeHmo( final File file ) throws Exception
  {
    hmoSerializer = new HMOSerializer( file );
    final IFeatureWrapperCollection<IFE1D2DElement> elements = m_discretisationModel1d2d.getElements();
    writeElements( elements );
    hmoSerializer.finish();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2DMeshConverter#writeMesh(java.io.File)
   */
  @Override
  public void writeMesh( final File file ) throws Exception
  {
    writeHmo( file );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  @Override
  public int getConversionID( IFeatureWrapper2 feature )
  {
    if( feature == null ) // TODO: this is probably an error in the data, throw an exception instead?
      return 0;

    final String id = feature.getGmlID();
    if( feature instanceof IFE1D2DNode )
      return m_nodesIDProvider.getOrAdd( id );

    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.INativeIDProvider#getConversionID(java.lang.String)
   */
  @Override
  public int getConversionID( String featureGmlID )
  {
    if( m_nodesIDProvider.contains( featureGmlID ) )
      return m_nodesIDProvider.getOrAdd( featureGmlID );

    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2DMeshConverter#supportFlowResistanceClasses()
   */
  @Override
  public boolean supportFlowResistanceClasses( )
  {
    return SUPPORT_FLOW_RESISTANCE_CLASSES;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.I2DMeshConverter#supportMidsideNodes()
   */
  @Override
  public boolean supportMidsideNodes( )
  {
    return SUPPORT_MIDSIDE_NODES;
  }
}
