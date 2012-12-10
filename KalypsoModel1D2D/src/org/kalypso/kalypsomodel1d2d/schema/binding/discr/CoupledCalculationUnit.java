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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Default implementation of {@link ICoupledCalculationUnit}
 * 
 * @author kurzbach
 * 
 */
public class CoupledCalculationUnit extends CalculationUnit implements ICoupledCalculationUnit
{
  public CoupledCalculationUnit( final Feature featureToBind, final QName qnameToBind, final QName elementListPropQName, final Class<IFENetItem> wrapperClass )
  {
    super( featureToBind, qnameToBind, elementListPropQName, wrapperClass );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#isCoupledSimulation()
   */
  @Override
  public boolean isCoupledSimulation( )
  {
    return FeatureHelper.booleanIsTrue( getFeature(), WB1D2D_PROP_COUPLED_SIMULATION, false );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#setCoupledSimulation()
   */
  @Override
  public void setCoupledSimulation( final boolean isCoupled )
  {
    setProperty( WB1D2D_PROP_COUPLED_SIMULATION, isCoupled );
  }
}
