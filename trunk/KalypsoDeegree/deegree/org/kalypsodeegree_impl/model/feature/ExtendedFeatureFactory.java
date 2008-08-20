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
package org.kalypsodeegree_impl.model.feature;

import java.lang.reflect.Constructor;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.Platform;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.KalypsoDeegreeExtensions;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBinding;
import org.osgi.framework.Bundle;

/**
 * Extended Feature Factory. Difference to old Feature Factory: looks for IFeatureBinding handlers and returns special
 * feature Implementations
 * 
 * @author Dirk Kuch
 */
public class ExtendedFeatureFactory
{
  private static ExtendedFeatureFactory SINGELTON = null;

  private ExtendedFeatureFactory( )
  {
  }

  public static ExtendedFeatureFactory getInstance( )
  {
    if( SINGELTON == null )
    {
      SINGELTON = new ExtendedFeatureFactory();
    }

    return SINGELTON;
  }

  public Feature getFeature( Feature parent, IRelationType parentRelation, IFeatureType featureType, String id, Object[] properties )
  {
    if( featureType == null )
      throw new IllegalArgumentException( "must provide a featuretype" );

    /* get feature binding class for feature type */
    IConfigurationElement element = KalypsoDeegreeExtensions.getFeatureBinding( featureType.getQName() );
    if( element != null )
    {
      try
      {
        String pluginid = element.getContributor().getName();
        Bundle bundle = Platform.getBundle( pluginid );
        Class< ? extends IFeatureBinding> featureClass = bundle.loadClass( element.getAttribute( "class" ) );
        Constructor< ? extends IFeatureBinding> constructor = featureClass.getConstructor( Object.class, IRelationType.class, IFeatureType.class, String.class, Object[].class );

        return constructor.newInstance( parent, parentRelation, featureType, id, properties );
      }
      catch( Throwable e )
      {
        e.printStackTrace();
      }
    }

    return new Feature_Impl( parent, parentRelation, featureType, id, properties );
  }
}
