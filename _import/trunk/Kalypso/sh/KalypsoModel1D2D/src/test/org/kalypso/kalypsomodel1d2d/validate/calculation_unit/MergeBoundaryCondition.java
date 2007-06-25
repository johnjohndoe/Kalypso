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
package test.org.kalypso.kalypsomodel1d2d.validate.calculation_unit;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.BoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IOperationalModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.OperationalModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitCheckDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 *
 */
public class MergeBoundaryCondition extends BoundaryCondition implements ICalculationValidateInterface
{

  private CalculationUnitDataModel dataModel;

  public CalculationUnitCheckDataModel checkDataModel;

  private List<ICalculationUnit> calculationUnits;

  public MergeBoundaryCondition( List<ICalculationUnit> calculationUnits, CalculationUnitDataModel dataModel, Feature featureToBind )
  {
    super( featureToBind );
    this.calculationUnits = calculationUnits;
    this.checkDataModel = checkDataModel;
    this.dataModel = dataModel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.calculation_unit.ICalculationValidateInterface#getBoundaryLines()
   */
  public List<IBoundaryLine> getBoundaryLines( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.calculation_unit.ICalculationValidateInterface#getCalculationUnit()
   */
  public List<ICalculationUnit> getCalculationUnit( )
  {
    return calculationUnits;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.validate.test.calculation_unit.ICalculationValidateInterface#checkAllInvariants()
   */
  public List< ? > checkAllInvariants( )
  {
    // TODO Auto-generated method stub
    return null;
  }
  
}
