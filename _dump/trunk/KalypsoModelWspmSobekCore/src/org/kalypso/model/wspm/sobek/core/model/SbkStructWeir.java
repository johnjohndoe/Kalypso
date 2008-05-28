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
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 */
public class SbkStructWeir extends SbkStructure implements ISbkStructWeir
{

  public SbkStructWeir( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir#getCrestLevel()
   */
  public double getCrestLevel( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_HEIGHT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir#getCrestHeight()
   */
  public double getCrestWidth( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR_CREST_WIDTH );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir#getDischargeCoeffCE()
   */
  public double getDischargeCoeffCE( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR_DISCHARGE_COEFF );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir#getFlowDirection()
   */
  public String getFlowDirection( )
  {
    // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR_FLOW_DIRECTION );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir#getLateralContractionCoeffCW()
   */
  public double getLateralContractionCoeffCW( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR_LATERAL_CONTRACTION_COEFF );
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
    return TYPE.eSbkStructWeir;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructWeir#setLinkToBranch()
   */
  public void setLinkToBranch( final IBranch branch ) throws Exception
  {
    FeatureUtils.setInternalLinkedFeature( getModel().getWorkspace(), getFeature(), ISobekConstants.QN_SBK_STRUCT_LINKS_TO_BRANCH, branch.getFeature() ); //$NON-NLS-1$
  }

}
