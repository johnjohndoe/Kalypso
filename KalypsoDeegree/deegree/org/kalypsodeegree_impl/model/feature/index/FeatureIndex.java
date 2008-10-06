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