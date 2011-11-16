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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Madanagopal
 */
public class ProblemDescriptor implements IProblem
{
  private String m_messageDescription;

  private String m_name;

  private ICalculationUnit m_calculationUnit;

  private final IFeatureWrapper2 m_featureToFocus;

  public ProblemDescriptor( final String name, final String messageDescription, final ICalculationUnit calculationUnit, final IFeatureWrapper2 featureToFocus )
  {
    m_name = name;
    m_messageDescription = messageDescription;
    m_calculationUnit = calculationUnit;
    m_featureToFocus = featureToFocus;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#getMessageDescription()
   */
  @Override
  public String getMessageDescription( )
  {
    return m_messageDescription;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#getName()
   */
  @Override
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#navigateToProblem(org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void navigateToProblem( final IMapPanel panel )
  {
    // @TODO Madan - Must change the below statement to show
    // the element that should get the Focus.. using showFocusOn()
    final GM_Envelope boundingBox = CalcUnitOps.getBoundingBox( getParentCalculationUnit() );
    if( boundingBox == null )
    {
      System.out.println( "BBox is null" ); //$NON-NLS-1$
      return;
    }
    panel.setBoundingBox( boundingBox );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setMessageDescription(java.lang.String)
   */
  @Override
  public void setMessageDescription( final String description )
  {
    m_messageDescription = description;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setName(java.lang.String)
   */
  @Override
  public void setName( final String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#getParentCalculationUnit()
   */
  @Override
  public ICalculationUnit getParentCalculationUnit( )
  {
    return m_calculationUnit;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setParentCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public void setParentCalculationUnit( final ICalculationUnit calculationUnit )
  {
    this.m_calculationUnit = calculationUnit;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setFocusOn(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  @Override
  public IFeatureWrapper2 showFocusOn( )
  {
    return m_featureToFocus;
  }
}
