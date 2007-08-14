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
package org.kalypso.kalypsomodel1d2d.schema.binding.metadata;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public interface ISimulationDescriptionCollection extends IFeatureWrapper2
{
  public IFeatureWrapperCollection<IModelDescriptor> getModelDescriptors( );

  public IFeatureWrapperCollection<ISimulationDescriptor> getSimulationDescriptors( );

  /**
   * This a Model descriptor for the given feature wrapper to this wrapper
   * 
   * @param modelFeatureWrapper
   *            the feature wrapper which descriptor is to be added to this collection.
   */
  public IModelDescriptor addModelDescriptor( IFeatureWrapper2 modelFeatureWrapper );

  /**
   * To get the existing descriptor entry for the given feature
   * 
   * @param featureWrapper2
   *            the feature which descriptor is to be get
   * @return an {@link IModelDescriptor} representing the existing descriptor entry for the feature or null if no entry
   *         is available for the feature
   * 
   */
  public IModelDescriptor getExistingEntry( IFeatureWrapper2 featureWrapper2 );
}
