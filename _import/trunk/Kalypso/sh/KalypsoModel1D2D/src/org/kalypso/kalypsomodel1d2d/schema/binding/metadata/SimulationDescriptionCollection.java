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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class SimulationDescriptionCollection extends AbstractFeatureBinder implements ISimulationDescriptionCollection
{
  private IFeatureWrapperCollection<IModelDescriptor> m_modelDescriptors;
  private IFeatureWrapperCollection<ISimulationDescriptor> m_simulationDescriptors;

  public SimulationDescriptionCollection( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
    m_modelDescriptors = new FeatureWrapperCollection<IModelDescriptor>(featureToBind, IModelDescriptor.class, Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODELDESCRIPTOR);
    m_simulationDescriptors = new FeatureWrapperCollection<ISimulationDescriptor>(featureToBind, ISimulationDescriptor.class, Kalypso1D2DSchemaConstants.SIMMETA_PROP_SIMDESCRIPTOR);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#getModelDescriptors()
   */
  public IFeatureWrapperCollection<IModelDescriptor> getModelDescriptors( )
  {
    return m_modelDescriptors;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptionCollection#getSimulationDescriptors()
   */
  public IFeatureWrapperCollection<ISimulationDescriptor> getSimulationDescriptors( )
  {
    return m_simulationDescriptors;
  }

}
