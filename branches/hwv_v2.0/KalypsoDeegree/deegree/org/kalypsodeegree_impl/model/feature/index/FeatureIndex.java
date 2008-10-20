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
package org.kalypsodeegree_impl.model.feature.index;

import java.util.Comparator;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * This class provides some functions to index features by one of their properties.
 * 
 * @author Holger Albert
 */
public class FeatureIndex
{
  /**
   * The constructor.
   */
  private FeatureIndex( )
  {
  }

  /**
   * This function indexes the features by the given property and returns a HashMap.
   * 
   * @param features
   *          The feature list, which should be indexed.
   * @param qname
   *          The qname of the property, which should be used for indexing.
   * @return The HashMap of the features with the property value as key, and the features as value.
   */
  public static Map<Object, Feature> indexFeature( FeatureList features, QName qname )
  {
    /* Create the visitor. */
    IndexByPropertyVisitor visitor = new IndexByPropertyVisitor( qname, false, null );

    /* Collect the features. */
    features.accept( visitor );

    /* Get the index. */
    Map<Object, Feature> index = visitor.getIndex();

    return index;
  }

  /**
   * This function indexes the features by the given property and returns a TreeMap with the given comparator.
   * 
   * @param features
   *          The feature list, which should be indexed.
   * @param qname
   *          The qname of the property, which should be used for indexing.
   * @param comparator
   *          The comparator, that should be used for the TreeMap.
   * @return The TreeMap of the features with the property value as key, and the features as value.
   */
  public static SortedMap<Object, Feature> indexFeatureSorted( FeatureList features, QName qname, Comparator<Object> comparator )
  {
    /* Create the visitor. */
    IndexByPropertyVisitor visitor = new IndexByPropertyVisitor( qname, true, comparator );

    /* Collect the features. */
    features.accept( visitor );

    /* Get the index. */
    Map<Object, Feature> index = visitor.getIndex();

    /* We know (because of the second parameter in the constructor of the visitor), that it will be a TreeMap. */
    TreeMap<Object, Feature> sortedIndex = (TreeMap<Object, Feature>) index;

    return sortedIndex;
  }
}