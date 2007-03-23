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
