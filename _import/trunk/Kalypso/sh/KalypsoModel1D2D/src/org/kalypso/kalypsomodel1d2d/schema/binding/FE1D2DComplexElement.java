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
package org.kalypso.kalypsomodel1d2d.schema.binding;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEComplexElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * The default implementation of the {@link IFE1D2DComplexElement} interface.
 * 
 * @author Patrice Congo
 *
 */
public class FE1D2DComplexElement 
        extends AbstractFeatureBinder 
        implements IFE1D2DComplexElement<IFE1D2DComplexElement, IFE1D2DElement> 
{
  
  private final IFeatureWrapperCollection<IFE1D2DComplexElement> containers;
  private final IFeatureWrapperCollection<IFE1D2DElement> elements;
  
  protected FE1D2DComplexElement( 
                          Feature featureToBind, 
                          QName qnameToBind ,
                          QName containerListPropQName,
                          QName elementListPropQName)
  {
    super(featureToBind, qnameToBind);
    if(containerListPropQName==null)
    {
      containers= null;
    }
    else
    {
      containers=
          Util.get( 
            featureToBind, 
            qnameToBind, 
            containerListPropQName, 
            IFE1D2DComplexElement.class, 
            true);
    }
    
    elements=Util.get( 
        featureToBind, 
        qnameToBind, 
        elementListPropQName, 
        IFE1D2DElement.class, 
        true);    
  }



  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEComplexElement#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getContainers( )
  {
    return containers;
  }



  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEComplexElement#getElements()
   */
  public IFeatureWrapperCollection<IFE1D2DElement> getElements( )
  {
    return elements;
  }



  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return getFeature();
  }

}
