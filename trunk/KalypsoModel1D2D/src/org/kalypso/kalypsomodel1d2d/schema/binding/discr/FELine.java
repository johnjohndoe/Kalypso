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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public abstract class FELine extends Feature_Impl implements IFELine
{
  protected final IFeatureBindingCollection<IFE1D2DNode> m_nodes = new FeatureBindingCollection<IFE1D2DNode>( this, IFE1D2DNode.class, PROP_NODES );
  
  private final IFeatureBindingCollection<IFE1D2DComplexElement> m_containers = new FeatureBindingCollection<IFE1D2DComplexElement>( this, IFE1D2DComplexElement.class, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS );

  public FELine( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine#setNodes(java.util.List)
   */
  @Override
  public abstract List<IFE1D2DNode> createFullNodesList( final List<IFE1D2DNode> nodes ) throws CoreException;

  protected void setGeometry( GM_Object value )
  {
    setProperty( IFELine.PROP_GEOMETRY, value );
  }

  @Override
  public List<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine#getContainers()
   */
  @Override
  public IFeatureBindingCollection<IFE1D2DComplexElement> getContainers( )
  {
    return m_containers;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine#getGeometry()
   */
  @Override
  public GM_Curve getGeometry( )
  {
    return (GM_Curve) getProperty( IFELine.PROP_GEOMETRY );
  }
}
