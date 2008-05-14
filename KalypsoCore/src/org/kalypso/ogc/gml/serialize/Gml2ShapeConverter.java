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
package org.kalypso.ogc.gml.serialize;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ISimpleMarshallingTypeHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.io.shpapi.dataprovider.IShapeDataProvider;

/**
 * Helps converting gml(-workspace) to shape files.
 * 
 * @author Gernot Belger
 */
public class Gml2ShapeConverter
{
  private final Map<String, QName> m_mapping;

  private final QName m_geomProp;

  public static Gml2ShapeConverter createDefault( final IFeatureType featureType )
  {
    final IPropertyType[] sourceProps = featureType.getProperties();
    final Map<String, QName> propertyMap = new HashMap<String, QName>( sourceProps.length );
    for( final IPropertyType sourceProp : sourceProps )
    {
      if( !sourceProp.isList() && sourceProp instanceof IValuePropertyType ) // TODO: consider gml:_name as well
      {
        final IValuePropertyType vpt = (IValuePropertyType) sourceProp;
        if( !vpt.isGeometry() )
        {
          final IMarshallingTypeHandler typeHandler = vpt.getTypeHandler();
          if( typeHandler instanceof ISimpleMarshallingTypeHandler )
          {
            final QName propName = vpt.getQName();
            propertyMap.put( propName.getLocalPart(), vpt.getQName() );
          }
        }
      }
    }

    /* also write gmlID */
    propertyMap.put( "gmlID", ShapeSerializer.QNAME_GMLID ); //$NON-NLS-1$

    final IValuePropertyType geomProp = featureType.getDefaultGeometryProperty();

    return new Gml2ShapeConverter( propertyMap, geomProp.getQName() );
  }

  public Gml2ShapeConverter( final Map<String, QName> mapping, final QName geomProp )
  {
    m_mapping = mapping;
    m_geomProp = geomProp;
  }

  public void writeShape( final FeatureList featureList, final String shapeFileBase, IShapeDataProvider shapeDataProvider, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString("org.kalypso.ogc.gml.serialize.Gml2ShapeConverter.1"), 2 ); //$NON-NLS-1$

    final Feature[] features = featureList.toFeatures();
    try
    {
      ShapeSerializer.serializeFeatures( features, m_mapping, m_geomProp, shapeFileBase, shapeDataProvider );
    }
    catch( final GmlSerializeException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();
    }

  }

}
