/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * <p>
 * Decorater over any FeatureVisitor, but only visits features of a given type.
 * </p>
 * <p>
 * Comparison is by name of the given type
 * </p>
 * 
 * @author belger
 */
public class FeatureSubstitutionVisitor implements FeatureVisitor
{
  private FeatureVisitor m_visitor;

  private final IFeatureType m_featureType;

  public FeatureSubstitutionVisitor( final FeatureVisitor visitor, final IFeatureType featureType )
  {
    m_visitor = visitor;
    m_featureType = featureType;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    if( matchesType( f ) )
      m_visitor.visit( f );

    return true;
  }

  public boolean matchesType( final Feature f )
  {
    final IFeatureType featureType = f.getFeatureType();

    return GMLSchemaUtilities.substitutes( featureType, m_featureType.getQName() );
  }

  public void setVisitor( final FeatureVisitor visitor )
  {
    m_visitor = visitor;
    
  }
}
