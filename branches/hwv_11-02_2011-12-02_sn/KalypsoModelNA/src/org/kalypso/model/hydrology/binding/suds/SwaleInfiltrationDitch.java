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
package org.kalypso.model.hydrology.binding.suds;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;

/**
 * @author Dirk Kuch
 */
public class SwaleInfiltrationDitch extends AbstractSwale implements ISwaleInfiltrationDitch
{
  public SwaleInfiltrationDitch( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch#getElementType()
   */
  @Override
  public String getElementType( )
  {
    final Object property = getProperty( QN_PROPERTY_ELEMENT_TYPE );
    if( property instanceof String )
      return (String) property;
    return "30"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch#getMaxPercRate()
   */
  @Override
  public double getMaxPercRate( )
  {
    final Object property = getProperty( QN_PROPERTY_MAX_PERC_RATE );
    if( property instanceof Double )
      return (Double) property;

    return 2.8E-15;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch#getPercentToGroundwater()
   */
  @Override
  public double getPercentToGroundwater( )
  {
    final Object property = getProperty( QN_PROPERTY_PERCENT_TO_GROUNDWATER );
    if( property instanceof Double )
      return (Double) property;

    return 1.0;
  }

  /**
   * @see org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale#getIdealLanduseName()
   */
  @Override
  public String getIdealLanduseName( )
  {
    return IDEAL_LANDUSE;
  }

}
