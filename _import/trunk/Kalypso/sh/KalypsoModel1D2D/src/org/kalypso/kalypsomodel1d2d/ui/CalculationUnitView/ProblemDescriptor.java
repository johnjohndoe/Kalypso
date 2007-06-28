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
package org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView;

import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Madanagopal
 *
 */
@SuppressWarnings("unchecked")
public class ProblemDescriptor implements IProblem
{

  private String messageDescription;
  private String name;
  private ICalculationUnit calculationUnit;
  private IFeatureWrapper2 featureToFocus;

  public ProblemDescriptor(String name, String messageDescription, ICalculationUnit calculationUnit , IFeatureWrapper2 featureToFocus)
  {
    this.name = name;
    this.messageDescription =  messageDescription;
    this.calculationUnit =calculationUnit;
    this.featureToFocus = featureToFocus;
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#getMessageDescription()
   */
  public String getMessageDescription( )
  {
    return messageDescription;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#getName()
   */
  public String getName( )
  {
    return name;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#navigateToProblem(org.kalypso.ogc.gml.map.MapPanel)
   */
  public void navigateToProblem( MapPanel panel )
  {
    GM_Envelope boundingBox = CalUnitOps.getBoundingBox( getParentCalculationUnit() );
    if( boundingBox == null )
    {
      System.out.println("BBox is null");
      return;
    }
    panel.setBoundingBox( boundingBox );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setMessageDescription(java.lang.String)
   */
  public void setMessageDescription( String description )
  {
    this.messageDescription = description;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setName(java.lang.String)
   */
  public void setName( String name )
  {
    this.name = name;
  }
  

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#getParentCalculationUnit()
   */
  public ICalculationUnit getParentCalculationUnit( )
  {
    return calculationUnit; 
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setParentCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  public void setParentCalculationUnit( ICalculationUnit calculationUnit )
  {
    this.calculationUnit = calculationUnit;
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.IProblem#setFocusOn(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public IFeatureWrapper2 showFocusOn()
  {
    return featureToFocus;
  }    
}
