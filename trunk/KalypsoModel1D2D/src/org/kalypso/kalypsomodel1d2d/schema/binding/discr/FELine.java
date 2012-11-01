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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public abstract class FELine extends Feature_Impl implements IFELine
{
  public FELine( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  protected FeatureList complexElementsInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_ELEMENT_CONTAINERS );
  }

  protected FeatureList nodesInternal( )
  {
    return (FeatureList)getProperty( PROP_NODES );
  }

  protected void setGeometry( final GM_Curve value )
  {
    setProperty( PROP_GEOMETRY, value );
    setEnvelopesUpdated();
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    return nodesInternal().toFeatures( new IFE1D2DNode[nodesInternal().size()] );
  }

  @Override
  public void addLinkedComplexElement( IFE1D2DComplexElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    if( !complexElementsInternal().containsOrLinksTo( element ) )
      complexElementsInternal().addLink( element );
  }

  @Override
  public void removeLinkedComplexElement( IFE1D2DComplexElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    if( complexElementsInternal().containsOrLinksTo( element ) )
      complexElementsInternal().removeLink( element );
  }

  @Override
  public IFE1D2DComplexElement[] getLinkedElements( )
  {
    return complexElementsInternal().toFeatures( new IFE1D2DComplexElement[complexElementsInternal().size()] );
  }

  @Override
  public GM_Curve getGeometry( )
  {
    return (GM_Curve)getProperty( IFELine.PROP_GEOMETRY );
  }
}
