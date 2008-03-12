/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core.model;

import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IGmlWorkspaces;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 */
public class SbkStructPump extends SbkStructure implements ISbkStructPump
{
  public SbkStructPump( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getCapacity()
   */
  public double getCapacity( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_CAPACITY );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getControlPosition()
   */
  public String getControlPosition( )
  {
    // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_PUMP_CONTROL );

  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getFlowDirection()
   */
  public String getFlowDirection( )
  {
    // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_FLOW_DIRECTION );

  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getReductionConstant()
   */
  public double getReductionConstant( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_CONSTANT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getReductionType()
   */
  public String getReductionType( )
  {
    // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_REDUCTION_TYPE );

  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getSwitchOffLevelPressureSide()
   */
  public double getSwitchOffLevelPressureSide( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_PRESSURE );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getSwitchOffLevelSuctionSide()
   */
  public double getSwitchOffLevelSuctionSide( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_OFF_SUCTION );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getSwitchOnLevelPressureSide()
   */
  public double getSwitchOnLevelPressureSide( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_PRESSURE );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#getSwitchOnLevelSuctionSide()
   */
  public double getSwitchOnLevelSuctionSide( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP_SWITCH_ON_SUCTION );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }
  
  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  @Override
  public TYPE getType( )
  {
    return TYPE.eSbkStructPump;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructPump#setLinkToBranch(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public void setLinkToBranch( IBranch branch ) throws Exception
  {
    FeatureUtils.updateLinkedFeature( getModel().getWorkspace(), getFeature(), ISobekConstants.QN_SBK_STRUCT_LINKS_TO_BRANCH, IGmlWorkspaces.HYDRAUL_MODEL + "#" + branch.getFeature().getId() );
  }
}
