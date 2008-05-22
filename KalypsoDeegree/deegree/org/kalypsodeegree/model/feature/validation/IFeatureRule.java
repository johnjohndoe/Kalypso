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
package org.kalypsodeegree.model.feature.validation;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public interface IFeatureRule extends IExecutableExtension
{
  /**
   * The unique id of this rule.
   */
  public String getID( );

  /**
   * A (human readable) name of this rule.
   */
  public String getName( );

  /**
   * A (human readable) description of this rule.
   */
  public String getDescription( );

  /**
   * The qname(s), this rule is responsible for. If <code>null</code> (not empty), this rule validates all types
   * of features.
   * <p>
   * A rule which specifies qnames, can excpect to validate only features of theses types.
   * </p>
   * 
   * @see #validate(Feature, IFeatureMarkerCollector)
   */
  public QName[] getQNames( );

  /**
   * Validate a feature (and its children).
   * <p>
   * Create problem markers if necessary.
   * </p>
   * 
   * @param feature
   *          The feature to validate. If {@link #getQNames()} returns something, only feature's whith these types may
   *          be given here.
   * @param collector
   *          Do not create problem markers yourself, give them to the collector instead.
   * @throws IllegalArgumentException
   *           If the type of the given featuee does not correspong to the specified qnames.
   * @see #getQNames()
   * @throws CoreException
   */
  public void validate( final Feature feature, final IFeatureMarkerCollector collector ) throws CoreException;
}
