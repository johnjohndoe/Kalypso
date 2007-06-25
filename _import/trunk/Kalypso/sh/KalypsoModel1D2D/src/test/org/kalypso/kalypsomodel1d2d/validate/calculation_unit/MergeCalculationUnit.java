/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitCheckDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 *
 */
public class MergeCalculationUnit extends CalculationUnit
{

  private CalculationUnitDataModel dataModel;
  private CalculationUnitCheckDataModel checkDataModel = new CalculationUnitCheckDataModel();

  public MergeCalculationUnit( Feature featureToBind, QName qnameToBind, QName elementListPropQName, Class wrapperClass )
  {
    super( featureToBind, qnameToBind, elementListPropQName, wrapperClass );
  }
  
  public MergeCalculationUnit(CalculationUnitDataModel dataModel,
      Feature featureToBind, 
      QName qnameToBind, 
      QName elementListPropQName, 
      Class wrapperClass )
  {
    this(featureToBind, qnameToBind, elementListPropQName, wrapperClass );
    this.dataModel = dataModel;
  }
  
  CommandableWorkspace workspace = 
    dataModel.getData( 
        CommandableWorkspace.class, 
        ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE );
  
  final Feature opModelFeature = workspace.getRootFeature();
  IFlowRelationshipModel opModel = 
    (IFlowRelationshipModel) opModelFeature.getAdapter( IFlowRelationshipModel.class );  

  
}
