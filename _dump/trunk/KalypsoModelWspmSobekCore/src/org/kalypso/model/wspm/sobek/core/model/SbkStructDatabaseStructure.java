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
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure;
import org.kalypso.model.wspm.sobek.core.interfaces.ISbkTable;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.INode.TYPE;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author thuel2
 */
public class SbkStructDatabaseStructure extends SbkStructure implements ISbkStructDatabaseStructure
{
  public SbkStructDatabaseStructure( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure#getCrestHeight()
   */
  public double getCrestHeight( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_CREST_HEIGHT );
    if( property instanceof Double )
      return (((Double) property).doubleValue());

    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure#getDatabase()
   */
  public ISbkTable getDatabase( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_MEMBER );
    if( property instanceof Feature )
      return new SbkTable( (Feature) property );

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure#getDatabaseUsage()
   */
  public ISbkTable getDatabaseUsage( )
  {
    final Object property = getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_DATABASE_USAGE_MEMBER );
    if( property instanceof Feature )
      return new SbkTable( (Feature) property );

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure#getInterpolationType()
   */
  public String getInterpolationType( )
  {
    // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_INTERPOLATION_TYPE );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure#getNumOfGateValues()
   */
  public long getNumOfGateValues( )
  {
    return (Long) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_NUMBER_GATE_VALUES );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.ISbkStructDatabaseStructure#getSecondAxisValueType()
   */
  public String getSecondAxisValueType( )
  {
    // TODO ggf. noch "Übersetzung" des Strings... von nofdp -> SBK
    return (String) getFeature().getProperty( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE_SECOND_AXIS_VALUE_TYPE );
  }
  
  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  @Override
  public TYPE getType( )
  {
    return TYPE.eSbkStructDatabaseStructure;
  }
}
