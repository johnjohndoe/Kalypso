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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * This visitor collects all features which link to a given href (=feature-id).
 * 
 * @author Gernot Belger
 */
public class FindLinkedFeatureVisitor implements FeatureVisitor
{
  private final String[] m_hrefs;

  private Map<Feature, Set<IRelationType>> m_linkedFeatures = new HashMap<Feature, Set<IRelationType>>();

  public FindLinkedFeatureVisitor( final String[] hrefs )
  {
    m_hrefs = hrefs;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final IPropertyType[] properties = f.getFeatureType().getProperties();
    for( final IPropertyType pt : properties )
    {
      if( pt instanceof IRelationType )
      {
        final IRelationType rt = (IRelationType) pt;
        if( rt.isList() )
        {
          final List linkList = (List) f.getProperty( rt );
          for( final Object link : linkList )
            checkFeature( f, rt, link );
        }
        else
        {
          final Object link = f.getProperty( rt );
          checkFeature( f, rt, link );
        }
      }
    }

    return true;
  }

  private void checkFeature( final Feature f, final IRelationType rt, final Object link )
  {
    for( final String href : m_hrefs )
    {
      if( link instanceof String && link.equals( href ) )
        addLink( f, rt );
      else if( link instanceof XLinkedFeature_Impl && href.equals( ((XLinkedFeature_Impl) link).getHref() ) )
        addLink( f, rt );
    }
  }

  private void addLink( final Feature f, final IRelationType rt )
  {
    if( !m_linkedFeatures.containsKey( f ) )
      m_linkedFeatures.put( f, new HashSet<IRelationType>() );

    final Set<IRelationType> rtSet = m_linkedFeatures.get( f );
    rtSet.add( rt );
  }

  public Map<Feature, Set<IRelationType>> getLinkedFeatures( )
  {
    return Collections.unmodifiableMap( m_linkedFeatures );
  }

}
