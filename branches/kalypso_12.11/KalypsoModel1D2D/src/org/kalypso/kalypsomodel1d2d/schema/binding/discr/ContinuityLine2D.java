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

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;

public class ContinuityLine2D extends FELine implements IContinuityLine2D
{
  public ContinuityLine2D( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList nodesInternal( )
  {
    return (FeatureList)getProperty( PROP_NODES );
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    return nodesInternal().toFeatures( new IFE1D2DNode[nodesInternal().size()] );
  }

  @Override
  public void setNodes( final IFE1D2DNode[] nodes )
  {
    final ContinuityLine2DGeometryBuilder builder = new ContinuityLine2DGeometryBuilder( nodes, new NullProgressMonitor() );

    /* set geometry */
    final GM_Curve geometry = builder.getCurve();
    setGeometry( geometry );

    /* set references to nodes */
    final FeatureList nodesInternal = nodesInternal();
    nodesInternal.clear();

    final IFE1D2DNode[] recalculatedNodes = builder.getContinuityNodes();
    for( final IFE1D2DNode node : recalculatedNodes )
      nodesInternal.addLink( node );

    setEnvelopesUpdated();
  }
}
