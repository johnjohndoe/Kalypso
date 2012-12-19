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
package org.kalypso.model.hydrology.internal.binding.timeseries;

import java.lang.ref.WeakReference;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.timeseries.IStationClass;
import org.kalypso.model.hydrology.binding.timeseries.IStationClasses;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

import com.google.common.base.Objects;

/**
 * @author Gernot Belger
 */
public class StationClasses extends Feature_Impl implements IStationClasses
{
  private static WeakReference<Map<Class< ? extends Station>, Set<String>>> HASH = new WeakReference<>( null );

  private static final QName MEMBER_CLASS = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "classMember" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<IStationClass> m_classes = new FeatureBindingCollection<>( this, IStationClass.class, MEMBER_CLASS );

  public StationClasses( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFeatureBindingCollection<IStationClass> getClasses( )
  {
    return m_classes;
  }

  public static synchronized String[] findAllowedParameterTypes( final Station station )
  {
    Assert.isNotNull( station );

    final Map<Class< ? extends Station>, Set<String>> classCatalog = getClassCatalog();
    final Set<String> allowedTypes = classCatalog.get( station.getClass() );
    if( allowedTypes == null )
      throw new IllegalArgumentException( String.format( Messages.getString("StationClasses_0"), station.getClass() ) ); //$NON-NLS-1$

    return allowedTypes.toArray( new String[allowedTypes.size()] );
  }

  private static Map<Class< ? extends Station>, Set<String>> getClassCatalog( )
  {
    final Map<Class< ? extends Station>, Set<String>> classCatalog = HASH.get();
    if( classCatalog != null )
      return classCatalog;

    final Map<Class< ? extends Station>, Set<String>> newClassCatalog = loadClassCatalog();
    HASH = new WeakReference<>( newClassCatalog );
    return newClassCatalog;
  }

  private static Map<Class< ? extends Station>, Set<String>> loadClassCatalog( )
  {
    final GMLWorkspace workspace = loadClassesWorkspace();
    final StationClasses stationClasses = (StationClasses) workspace.getRootFeature();

    final Map<Class< ? extends Station>, Set<String>> classCatalog = new HashMap<>();

    final IFeatureBindingCollection<IStationClass> classes = stationClasses.getClasses();
    for( final IStationClass stationClass : classes )
    {
      try
      {
        final String className = stationClass.getClassName();

        final Class< ? > loadedClass = StationClasses.class.getClassLoader().loadClass( className );
        if( loadedClass == null || !Station.class.isAssignableFrom( loadedClass ) )
          throw new IllegalArgumentException( String.format( "Class must inherit from Station: %s", Objects.firstNonNull( loadedClass, stationClass.getClass() ).getName() ) ); //$NON-NLS-1$

        @SuppressWarnings("unchecked")
        final Class< ? extends Station> verifiedClass = (Class< ? extends Station>) loadedClass;

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
      final String uri = KalypsoCorePlugin.getDefault().getCatalogManager().resolve( STATION_CLASSES_URN, STATION_CLASSES_URN );

      if( uri.startsWith( "urn:" ) ) //$NON-NLS-1$
      {
        // id was not found in catalog, what to do?
        throw new IllegalArgumentException( Messages.getString("StationClasses_2") + STATION_CLASSES_URN ); //$NON-NLS-1$
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
}