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
package org.kalypso.ui.rrm.internal.timeseries.binding;

import java.lang.ref.WeakReference;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class StationClasses extends Feature_Impl
{
  private static final String STATION_CLASSES_URN = "urn:ogc:gml:kalypso:model:rrm:stationClasses"; //$NON-NLS-1$

  private static WeakReference<Map<Class< ? extends Station>, Set<String>>> m_hash = new WeakReference<Map<Class< ? extends Station>, Set<String>>>( null );

  final static QName FEATURE_STATION_CLASSES = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "StationClasses" ); //$NON-NLS-1$

  private static final QName MEMBER_CLASS = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "classMember" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<StationClass> m_classes = new FeatureBindingCollection<StationClass>( this, StationClass.class, MEMBER_CLASS );

  public StationClasses( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  IFeatureBindingCollection<StationClass> getClasses( )
  {
    return m_classes;
  }

  public static synchronized IAxis[] findAllowedClasses( final Station station )
  {
    Assert.isNotNull( station );

    final Map<Class< ? extends Station>, Set<String>> classCatalog = getClassCatalog();
    final Set<String> allowedTypes = classCatalog.get( station.getClass() );
    if( allowedTypes == null )
      throw new IllegalArgumentException( String.format( "Unknown station class: %s", station.getClass() ) );

    return buildAxes( allowedTypes.toArray( new String[allowedTypes.size()] ) );
  }

  private static Map<Class< ? extends Station>, Set<String>> getClassCatalog( )
  {
    final Map<Class< ? extends Station>, Set<String>> classCatalog = m_hash.get();
    if( classCatalog != null )
      return classCatalog;

    final Map<Class< ? extends Station>, Set<String>> newClassCatalog = loadClassCatalog();
    m_hash = new WeakReference<Map<Class< ? extends Station>, Set<String>>>( newClassCatalog );
    return newClassCatalog;
  }

  private static Map<Class< ? extends Station>, Set<String>> loadClassCatalog( )
  {
    final GMLWorkspace workspace = loadClassesWorkspace();
    final StationClasses stationClasses = (StationClasses) workspace.getRootFeature();

    final Map<Class< ? extends Station>, Set<String>> classCatalog = new HashMap<>();

    final IFeatureBindingCollection<StationClass> classes = stationClasses.getClasses();
    for( final StationClass stationClass : classes )
    {
      try
      {
        final String className = stationClass.getClassName();

        final Class< ? > loadedClass = StationClasses.class.getClassLoader().loadClass( className );
        if( loadedClass == null || !loadedClass.isInstance( Station.class ) )
          throw new IllegalArgumentException( String.format( "Class must inherit from Station: %s", loadedClass.getName() ) ); //$NON-NLS-1$

        @SuppressWarnings("unchecked")
        final Class< ? extends Station> verifiedClass = (Class< ? extends Station>) loadedClass;

        final String parameterType = stationClass.getParameterType();

        if( !classCatalog.containsKey( verifiedClass ) )
          classCatalog.put( verifiedClass, new HashSet<String>() );

        final Set<String> types = classCatalog.get( verifiedClass );
        types.add( parameterType );
      }
      catch( final ClassNotFoundException e )
      {
        e.printStackTrace();
      }
    }

    return classCatalog;
  }

  private static GMLWorkspace loadClassesWorkspace( )
  {
    try
    {
      final ICatalog baseCatalog = KalypsoCorePlugin.getDefault().getCatalogManager().getBaseCatalog();
      final String uri = baseCatalog.resolve( STATION_CLASSES_URN, STATION_CLASSES_URN );

      if( uri.startsWith( "urn:" ) )
      {
        // id was not found in catalog, what to do?
        throw new IllegalArgumentException( "Unknown dictionary: " + STATION_CLASSES_URN );
      }

      final URL url = new URI( uri ).toURL();
      return GmlSerializer.createGMLWorkspace( url, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private static IAxis[] buildAxes( final String[] types )
  {
    final IAxis[] axes = new IAxis[types.length];

    for( int i = 0; i < axes.length; i++ )
      axes[i] = TimeseriesUtils.createDefaultAxis( types[i] );

    return axes;
  }
}