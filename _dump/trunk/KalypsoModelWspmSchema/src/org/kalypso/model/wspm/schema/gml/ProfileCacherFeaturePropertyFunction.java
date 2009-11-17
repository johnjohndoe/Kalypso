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
package org.kalypso.model.wspm.schema.gml;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResultUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author Gernot Belger
 */
public class ProfileCacherFeaturePropertyFunction extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Properties)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: interpolate geometry onto profile points?
    // see IProfilPointProperty.doInterpolation(value1,value2)
    return null;
  }

  // $ANALYSIS-IGNORE
  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    try
    {
      final IProfileFeature profile = (IProfileFeature) feature;
      return profile.getLine();
// return ProfileFeatureBinding.toLine( feature );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }



  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public static GM_Point convertPoint( final IProfil profile, final IRecord profilPoint, final String crs ) throws Exception
  {
    final int compRechtswert = TupleResultUtilities.indexOfComponent( profile, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final int compHochwert = TupleResultUtilities.indexOfComponent( profile, IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final int compHoehe = TupleResultUtilities.indexOfComponent( profile, IWspmConstants.POINT_PROPERTY_HOEHE );

    if( compRechtswert == -1 || compHochwert == -1 )
      return null;

    final Double rw = (Double) profilPoint.getValue( compRechtswert );
    final Double hw = (Double) profilPoint.getValue( compHochwert );
    final Double h = compHoehe == -1 ? null : (Double) profilPoint.getValue( compHoehe );

    return WspmGeometryUtilities.pointFromRwHw( rw, hw, h, crs, WspmGeometryUtilities.GEO_TRANSFORMER );
  }
}