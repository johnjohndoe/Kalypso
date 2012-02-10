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
package org.kalypso.risk.model.schema.propertyFunctions;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Dejan Antanaskovic
 */
public class PF_LandusePolygon_DamageFunction extends FeaturePropertyFunction
{
  private final static QName XLINKED_LANDUSE_CLS = new QName( KalypsoRiskSchemaCatalog.NS_VECTOR_DATA_MODEL, "landuseClassLink" ); //$NON-NLS-1$

  private final static QName XLINKED_DAMAGE_FUNCTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionLink" ); //$NON-NLS-1$

  private final static QName DAMAGE_FUNCTION_PROPERTY = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "function" ); //$NON-NLS-1$

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    try
    {
      final Feature landuseClass = (Feature) feature.getProperty( XLINKED_LANDUSE_CLS );
      if( landuseClass == null )
        return ""; //$NON-NLS-1$
      else
      {
        final XLinkedFeature_Impl landuseXlink = (XLinkedFeature_Impl) landuseClass;
        final GMLWorkspace controlWorkspace = landuseXlink.getFeature().getWorkspace();

        final Feature damageFunction = FeatureHelper.resolveLinkedFeature( controlWorkspace, landuseClass.getProperty( XLINKED_DAMAGE_FUNCTION ) );
        if( damageFunction == null )
          return ""; //$NON-NLS-1$
        else
        {
          final Object object = damageFunction.getProperty( DAMAGE_FUNCTION_PROPERTY );
          if( object instanceof List )
            return ((List<Object>) object).get( 0 );
          else
            return object;
        }
      }
    }
    catch( IllegalStateException e )
    {
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    return null;
  }

}
