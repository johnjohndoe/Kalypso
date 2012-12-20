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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.model.hydrology.binding.parameter.SoilLayerParameter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

/**
 * @author Dirk Kuch
 */
public class ValidSoilParametersVisitor<T extends SoilLayerParameter> implements IFeatureBindingCollectionVisitor<T>
{
  private final Set<SoilLayerParameter> m_valid = new LinkedHashSet<>();

  private final Set<SoilLayerParameter> m_invalid = new LinkedHashSet<>();

  @Override
  public void visit( final T parameter )
  {
    final Feature linkedSoilLayer = parameter.getLinkedSoilLayer();
    if( linkedSoilLayer == null )
      m_invalid.add( parameter );
    else
      m_valid.add( parameter );
  }

  public SoilLayerParameter[] getValidParameters( )
  {
    return m_valid.toArray( new SoilLayerParameter[] {} );
  }

  public SoilLayerParameter[] getInvalidParameters( )
  {
    return m_invalid.toArray( new SoilLayerParameter[] {} );
  }
}