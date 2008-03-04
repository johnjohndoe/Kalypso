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
package org.kalypso.model.wspm.sobek.core.model;

import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 */
public class SbkStructGeneralStructure extends SbkStructure implements ISbkStructGeneralStructure
{
  public SbkStructGeneralStructure( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getBedLevelDownstream()
   */
  public double getBedLevelDownstream( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_DOWNSTREAM );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getBedLevelStructure()
   */
  public double getBedLevelStructure( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getBedLevelStructureLeft()
   */
  public double getBedLevelStructureLeft( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_LEFT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getBedLevelStructureRight()
   */
  public double getBedLevelStructureRight( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_STRUCTURE_RIGHT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getBedLevelUpstream()
   */
  public double getBedLevelUpstream( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_BED_LEVEL_UPSTREAM );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getExtraResistence()
   */
  public double getExtraResistence( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_EXTRA_RESISTENCE );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getGateHeight()
   */
  public double getGateHeight( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_GATE_HEIGHT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getNegContractionCoeff()
   */
  public double getNegContractionCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_CONTRACTION_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getNegDrownedGateFlowCoeff()
   */
  public double getNegDrownedGateFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_GATE_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getNegDrownedWeirFlowCoeff()
   */
  public double getNegDrownedWeirFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_DROWNED_WEIR_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getNegFreeGateFlowCoeff()
   */
  public double getNegFreeGateFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_GATE_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getNegFreeWeirFlowCoeff()
   */
  public double getNegFreeWeirFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_NEG_FREE_WEIR_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getPosContractionCoeff()
   */
  public double getPosContractionCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_CONTRACTION_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getPosDrownedGateFlowCoeff()
   */
  public double getPosDrownedGateFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_GATE_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getPosDrownedWeirFlowCoeff()
   */
  public double getPosDrownedWeirFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_DROWNED_WEIR_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getPosFreeGateFlowCoeff()
   */
  public double getPosFreeGateFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_GATE_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getPosFreeWeirFlowCoeff()
   */
  public double getPosFreeWeirFlowCoeff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_POS_FREE_WEIR_FLOW_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getWidthDownstream()
   */
  public double getWidthDownstream( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_DOWNSTREAM );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getWidthStructure()
   */
  public double getWidthStructure( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getWidthStructureLeft()
   */
  public double getWidthStructureLeft( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_LEFT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getWidthStructureRight()
   */
  public double getWidthStructureRight( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_STRUCTURE_RIGHT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructGeneralStructure#getWidthUpstream()
   */
  public double getWidthUpstream( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE_WIDTH_UPSTREAM );
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
    return TYPE.eSbkStructGeneralStructure;
  }
}
