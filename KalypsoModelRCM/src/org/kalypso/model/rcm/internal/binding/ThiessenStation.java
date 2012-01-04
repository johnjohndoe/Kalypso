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
package org.kalypso.model.rcm.internal.binding;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IThiessenStation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Gernot Belger
 */
public class ThiessenStation extends Feature_Impl implements IThiessenStation
{
  public ThiessenStation( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public boolean isActive( )
  {
    return getBoolean( PROPERTY_ACTIVE, false );
  }

  @Override
  public void setActive( final boolean active )
  {
    setProperty( PROPERTY_ACTIVE, active );
  }

  @SuppressWarnings("unchecked")
  @Override
  public GM_Surface<GM_SurfacePatch> getThiessenArea( )
  {
    return getProperty( PROPERTY_THIESSEN_AREA, GM_Surface.class );
  }

  @Override
  public void setThiessenArea( final GM_Surface< ? > area )
  {
    setProperty( PROPERTY_THIESSEN_AREA, area );
  }

  @Override
  public Feature getStation( )
  {
    return getMember( LINK_STATION );
  }

  @Override
  public void setStation( final String href )
  {
    final IRelationType parentRelation = (IRelationType) getFeatureType().getProperty( LINK_STATION );
    final XLinkedFeature_Impl link = new XLinkedFeature_Impl( this, parentRelation, parentRelation.getTargetFeatureType(), href );
    setProperty( LINK_STATION, link );
  }
}