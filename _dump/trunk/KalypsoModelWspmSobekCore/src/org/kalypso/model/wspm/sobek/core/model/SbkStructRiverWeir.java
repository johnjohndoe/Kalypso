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

import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 *
 */
public class SbkStructRiverWeir extends SbkStructure implements ISbkStructRiverWeir
{
  public SbkStructRiverWeir( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getCrestLevel()
   */
  public double getCrestLevel( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_HEIGHT );
    if( property instanceof Double )
      return (((Double)property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getCrestShape()
   */
  public String getCrestShape( )
  {
 // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String)getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_SHAPE );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getCrestWidth()
   */
  public double getCrestWidth( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_CREST_WIDTH );
    if( property instanceof Double )
      return (((Double)property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getNegCorrectionCeoff()
   */
  public double getNegCorrectionCeoff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_CORRECTION_COEFF );
    if( property instanceof Double )
      return (((Double)property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getNegSubmergeLimit()
   */
  public double getNegSubmergeLimit( )
  {
    final Object property = getFeature().getProperty( ISobekConstants. QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_SUBMERGE_LIMIT);
    if( property instanceof Double )
      return (((Double)property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getPosCorrectionCeoff()
   */
  public double getPosCorrectionCeoff( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_CORRECTION_COEFF );
    if( property instanceof Double )
      return (((Double)property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getPosSubmergeLimit()
   */
  public double getPosSubmergeLimit( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_SUBMERGE_LIMIT );
    if( property instanceof Double )
      return (((Double)property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getNegReductionFactors()
   */
  public ISbkTable getNegReductionFactors( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_NEG_REDUCTION_FACTORS );
    if( property instanceof Feature )
      return new SbkTable((Feature)property);
    
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructRiverWeir#getPosReductionFactors()
   */
  public ISbkTable getPosReductionFactors( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR_POS_REDUCTION_FACTORS );
    if( property instanceof Feature )
      return new SbkTable((Feature)property);
    
    return null;
  }
}
