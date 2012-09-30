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
package org.kalypso.model.hydrology.timeseries;

import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.URIUtil;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.binding.timeseries.IHydrologicalStation;
import org.kalypso.model.hydrology.binding.timeseries.IMeteorologicalStation;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationClass;
import org.kalypso.model.hydrology.binding.timeseries.IStationClasses;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import com.google.common.base.Objects;

/**
 * @author Gernot Belger
 */
public class StationClassesCatalog
{
  private StationClassesCatalog( )
  {
  }

  private static WeakReference<Map<Class< ? extends IStation>, Set<String>>> HASH = new WeakReference<>( null );

  public static String[] findAllowedParameterTypes( final IStation station )
  {
    Assert.isNotNull( station );

    final Map<Class< ? extends IStation>, Set<String>> classCatalog = getClassCatalog();

    final Class< ? >[] interfaces = station.getClass().getInterfaces();
    for( final Class< ? > iface : interfaces )
    {
      final Set<String> allowedTypes = classCatalog.get( iface );
      if( allowedTypes != null && !allowedTypes.isEmpty() )
        return allowedTypes.toArray( new String[allowedTypes.size()] );
    }

    throw new IllegalArgumentException( String.format( Messages.getString("StationClassesCatalog_0"), station.getClass() ) ); //$NON-NLS-1$

  }

  public static String[] findAllowedParameterTypes( final Class< ? > clazz )
  {
    Assert.isNotNull( clazz );

    final Map<Class< ? extends IStation>, Set<String>> classCatalog = getClassCatalog();

    final Set<String> allowedTypes = classCatalog.get( clazz );
    if( allowedTypes == null )
      throw new IllegalArgumentException( String.format( Messages.getString("StationClassesCatalog_1"), clazz ) ); //$NON-NLS-1$

    return allowedTypes.toArray( new String[allowedTypes.size()] );
  }

  public static QName getTypeFor( final String parameterType )
  {
    final Map<Class< ? extends IStation>, Set<String>> classCatalog = getClassCatalog();

    for( final Entry<Class< ? extends IStation>, Set<String>> entry : classCatalog.entrySet() )
    {
      if( entry.getValue().contains( parameterType ) )
      {
        final Class< ? extends IStation> key = entry.getKey();
        if( IMeteorologicalStation.class.isAssignableFrom( key ) )
          return IMeteorologicalStation.FEATURE_METEOROLOGICAL_STATION;
        if( IHydrologicalStation.class.isAssignableFrom( key ) )
          return IHydrologicalStation.FEATURE_HYDROLOGICAL_STATION;
      }
    }

    return null;
  }

  private static synchronized Map<Class< ? extends IStation>, Set<String>> getClassCatalog( )
  {
    final Map<Class< ? extends IStation>, Set<String>> classCatalog = HASH.get();
    if( classCatalog != null )
      return classCatalog;

    final Map<Class< ? extends IStation>, Set<String>> newClassCatalog = loadClassCatalog();
    HASH = new WeakReference<>( newClassCatalog );
    return newClassCatalog;
  }

  private static Map<Class< ? extends IStation>, Set<String>> loadClassCatalog( )
  {
    final GMLWorkspace workspace = loadClassesWorkspace();
    final IStationClasses stationClasses = (IStationClasses) workspace.getRootFeature();

    final Map<Class< ? extends IStation>, Set<String>> classCatalog = new HashMap<>();

    final IFeatureBindingCollection<IStationClass> classes = stationClasses.getClasses();
    for( final IStationClass stationClass : classes )
    {
      try
      {
        final String className = stationClass.getClassName();

        final Class< ? > loadedClass = StationClassesCatalog.class.getClassLoader().loadClass( className );
        if( loadedClass == null || !IStation.class.isAssignableFrom( loadedClass ) )
          throw new IllegalArgumentException( String.format( "Class must inherit from Station: %s", Objects.firstNonNull( loadedClass, stationClass.getClass() ).getName() ) ); //$NON-NLS-1$

        @SuppressWarnings("unchecked")
        final Class< ? extends IStation> verifiedClass = (Class< ? extends IStation>) loadedClass;

        if( !classCatalog.containsKey( verifiedClass ) )
          classCatalog.put( verifiedClass, new HashSet<String>() );

        final Set<String> types = classCatalog.get( verifiedClass );

        final String[] parameterTypes = stationClass.getParameterTypes();
        types.addAll( Arrays.asList( parameterTypes ) );
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
      final String uri = KalypsoCorePlugin.getDefault().getCatalogManager().resolve( IStationClasses.STATION_CLASSES_URN, IStationClasses.STATION_CLASSES_URN );

      if( uri.startsWith( "urn:" ) ) //$NON-NLS-1$
      {
        // id was not found in catalog, what to do?
        throw new IllegalArgumentException( Messages.getString("StationClassesCatalog_3") + IStationClasses.STATION_CLASSES_URN ); //$NON-NLS-1$
      }

      final URL url = URIUtil.fromString( uri ).toURL();
      return GmlSerializer.createGMLWorkspace( url, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}