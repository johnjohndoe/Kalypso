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

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Default implementation of {@link ICalculationUnit2D}
 * 
 * @author Patrice Congo
 * 
 */
@SuppressWarnings("unchecked") //$NON-NLS-1$
public class CalculationUnit1D2D<ET extends IFE1D2DElement> extends CalculationUnit<ET> implements ICalculationUnit1D2D<ET>
{

  private final IFeatureWrapperCollection<ICalculationUnit> m_subCalculationUnits;

  private final IFeatureWrapperCollection<ET> m_elements;

  public CalculationUnit1D2D( Feature featureToBind )
  {
    this( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D2D, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS, Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT, (Class<ET>) IFENetItem.class );

  }

  public CalculationUnit1D2D( Feature featureToBind, QName qnameToBind, QName elementListPropQName, QName subUnitPropQName, Class<ET> wrapperClass )
  {
    super( featureToBind, qnameToBind, elementListPropQName, wrapperClass );
    m_subCalculationUnits = Util.<ICalculationUnit> get( featureToBind, qnameToBind, subUnitPropQName, ICalculationUnit.class, true );
    m_elements = new FeatureWrapperCollection( featureToBind, IFE1D2DElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );
    ((FeatureWrapperCollection) m_elements).addSecondaryWrapper( IFELine.class );
    m_elements.clear();
    for( final ICalculationUnit calculationUnit : m_subCalculationUnits )
      m_elements.addAll( calculationUnit.getElements() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DComplexElement#getElements()
   */
  @Override
  public IFeatureWrapperCollection<ET> getElements( )
  {
    return m_elements;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D#getSubUnits()
   */
  public IFeatureWrapperCollection<ICalculationUnit> getSubUnits( )
  {
    return m_subCalculationUnits;
  }

}
