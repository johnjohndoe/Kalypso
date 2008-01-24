package org.kalypso.model.wspm.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.model.wspm.core.gml.IProfileFeatureProvider;
import org.kalypso.model.wspm.core.profil.IProfilBuilder;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObjectProvider;
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;
import org.kalypso.model.wspm.core.profil.reparator.IProfilReparator;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;

/** Helper class to read extension points of this plugin. */
public class KalypsoModelWspmCoreExtensions
{
  private static IProfileFeatureProvider[] PROFILE_FEATURE_PROVIDER = null;

  private static IProfilePointFilter[] PROFILE_POINT_FILTER = null;

  private static Map<String, List<IProfilPointMarkerProvider>> THE_MARKER_PROVIDER_MAP = null;

  private static Map<String, List<IProfilPointPropertyProvider>> THE_PROPERTY_PROVIDER_MAP = null;

  private static Map<String, IProfilBuilder> THE_PROFIL_BUILDER_MAP = null;

  private static Map<String, List<IProfileObjectProvider>> THE_OBJECT_PROVIDER_MAP = null;

  public static IProfilReparator[] createReaparatorRules( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.reparatorrule" );

    final Collection<IProfilReparator> reparators = new ArrayList<IProfilReparator>( elements.length );
    final Collection<IStatus> stati = new ArrayList<IStatus>( elements.length );

    for( final IConfigurationElement element : elements )
    {
      try
      {
        final IProfilReparator rule = (IProfilReparator) element.createExecutableExtension( "class" );
        reparators.add( rule );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();

        stati.add( e.getStatus() );
      }
    }

    if( stati.size() > 0 )
    {
      final IStatus[] childrens = stati.toArray( new IStatus[stati.size()] );
      final IStatus status = new MultiStatus( KalypsoModelWspmCorePlugin.getID(), 0, childrens, "Ein oder mehrere Reparatoren konnten nicht initialisiert werden.", null );
      if( status != null )
      {
        // TODO: what to do whith this status?
      }
    }

    return reparators.toArray( new IProfilReparator[reparators.size()] );
  }

  public static IProfilSink createProfilSink( final String fileExtension ) throws CoreException
  {
    final Map<String, IConfigurationElement> sinkMap = getSinksOrSources( "sink" );

    final IConfigurationElement element = sinkMap.get( fileExtension );
    if( element == null )
      return null;

    return (IProfilSink) element.createExecutableExtension( "class" );
  }

  /**
   * @param fileExtension
   *            File extension without '.'
   */
  public static IProfilSource createProfilSource( final String fileExtension ) throws CoreException
  {
    final Map<String, IConfigurationElement> sinkMap = getSinksOrSources( "source" );

    final IConfigurationElement element = sinkMap.get( fileExtension );
    if( element == null )
      return null;

    return (IProfilSource) element.createExecutableExtension( "class" );
  }

  private static Map<String, IConfigurationElement> getSinksOrSources( final String name )
  {
    final IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = extensionRegistry.getConfigurationElementsFor( "org.kalypso.model.wspm.core", "profilserializer" );
    final Map<String, IConfigurationElement> map = new HashMap<String, IConfigurationElement>( elements.length );
    for( final IConfigurationElement element : elements )
    {
      final String eltName = element.getName();
      if( eltName.equals( name ) )
      {
        final String ext = element.getAttribute( "extension" );
        map.put( ext, element );
      }
    }

    return map;
  }

  public synchronized static IProfileFeatureProvider[] getProfileFeatureProvider( )
  {
    if( PROFILE_FEATURE_PROVIDER != null )
      return PROFILE_FEATURE_PROVIDER;

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profileFeatureProvider" );

    final Collection<IProfileFeatureProvider> provider = new ArrayList<IProfileFeatureProvider>( elements.length );
    for( final IConfigurationElement element : elements )
    {
      try
      {
        provider.add( (IProfileFeatureProvider) element.createExecutableExtension( "class" ) );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    PROFILE_FEATURE_PROVIDER = provider.toArray( new IProfileFeatureProvider[provider.size()] );

    return PROFILE_FEATURE_PROVIDER;
  }

  public static IProfilePointFilter[] getProfilePointFilters( )
  {
    if( PROFILE_POINT_FILTER != null )
      return PROFILE_POINT_FILTER;

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] elements = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profilePointFilter" );

    final Collection<IProfilePointFilter> filter = new ArrayList<IProfilePointFilter>( elements.length );
    for( final IConfigurationElement element : elements )
    {
      try
      {
        filter.add( (IProfilePointFilter) element.createExecutableExtension( "class" ) );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    PROFILE_POINT_FILTER = filter.toArray( new IProfilePointFilter[filter.size()] );

    return PROFILE_POINT_FILTER;
  }

  public static IProfilPointMarkerProvider[] getMarkerProviders( final String profilType )
  {
    final Map<String, List<IProfilPointMarkerProvider>> map = getMarkerProviders();
    final List<IProfilPointMarkerProvider> list = map.get( profilType );
    if( list == null )
      return new IProfilPointMarkerProvider[0];

    return list.toArray( new IProfilPointMarkerProvider[list.size()] );
  }

  public static IProfilPointMarkerProvider[] getAllMarkerProviders( )
  {
    final Map<String, List<IProfilPointMarkerProvider>> map = getMarkerProviders();
    final ArrayList<IProfilPointMarkerProvider> list = new ArrayList<IProfilPointMarkerProvider>();
    for( final List<IProfilPointMarkerProvider> ppmp : map.values() )
    {
      list.addAll( ppmp );
    }
    if( list == null )
      return new IProfilPointMarkerProvider[0];

    return list.toArray( new IProfilPointMarkerProvider[list.size()] );
  }

  private static synchronized Map<String, List<IProfilPointMarkerProvider>> getMarkerProviders( )
  {
    if( THE_MARKER_PROVIDER_MAP != null )
      return THE_MARKER_PROVIDER_MAP;

    THE_MARKER_PROVIDER_MAP = new HashMap<String, List<IProfilPointMarkerProvider>>();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] markerProvider = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profilPointMarkerProvider" );
    for( final IConfigurationElement configurationElement : markerProvider )
    {
      try
      {
        final String profilType = configurationElement.getAttribute( "profiletype" );
        final Object protoProvider = configurationElement.createExecutableExtension( "provider" );
        final IProfilPointMarkerProvider provider = (IProfilPointMarkerProvider) protoProvider;

        if( !THE_MARKER_PROVIDER_MAP.containsKey( profilType ) )
          THE_MARKER_PROVIDER_MAP.put( profilType, new ArrayList<IProfilPointMarkerProvider>() );

        THE_MARKER_PROVIDER_MAP.get( profilType ).add( provider );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return THE_MARKER_PROVIDER_MAP;
  }

  public static IProfileObjectProvider[] getObjectProviders( final String profilType )
  {
    final Map<String, List<IProfileObjectProvider>> map = getObjectProviders();
    final List<IProfileObjectProvider> list = map.get( profilType );
    if( list == null )
      return new IProfileObjectProvider[0];

    return list.toArray( new IProfileObjectProvider[list.size()] );
  }

  private static synchronized Map<String, List<IProfileObjectProvider>> getObjectProviders( )
  {
    if( THE_OBJECT_PROVIDER_MAP != null )
      return THE_OBJECT_PROVIDER_MAP;

    THE_OBJECT_PROVIDER_MAP = new HashMap<String, List<IProfileObjectProvider>>();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] objectProvider = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profileObjectProvider" );
    for( final IConfigurationElement configurationElement : objectProvider )
    {
      try
      {
        final String profilType = configurationElement.getAttribute( "profiletype" );
        final Object protoProvider = configurationElement.createExecutableExtension( "provider" );
        final IProfileObjectProvider provider = (IProfileObjectProvider) protoProvider;

        if( !THE_OBJECT_PROVIDER_MAP.containsKey( profilType ) )
          THE_OBJECT_PROVIDER_MAP.put( profilType, new ArrayList<IProfileObjectProvider>() );

        THE_OBJECT_PROVIDER_MAP.get( profilType ).add( provider );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return THE_OBJECT_PROVIDER_MAP;
  }

  public static IProfilPointPropertyProvider[] getPointPropertyProviders( final String profilType )
  {
    final Map<String, List<IProfilPointPropertyProvider>> map = getPointPropertyProviders();
    final List<IProfilPointPropertyProvider> list = map.get( profilType );
    if( list == null )
      return new IProfilPointPropertyProvider[0];

    return list.toArray( new IProfilPointPropertyProvider[list.size()] );
  }

  private static synchronized Map<String, List<IProfilPointPropertyProvider>> getPointPropertyProviders( )
  {
    if( THE_PROPERTY_PROVIDER_MAP != null )
      return THE_PROPERTY_PROVIDER_MAP;

    THE_PROPERTY_PROVIDER_MAP = new HashMap<String, List<IProfilPointPropertyProvider>>();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] propertyProvider = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profilPointPropertyProvider" );
    for( final IConfigurationElement configurationElement : propertyProvider )
    {
      try
      {
        // TODO: eventuell nur die elemente des gewünschten typs erzeugen und in map merken
        final String profilType = configurationElement.getAttribute( "profiletype" );
        final Object protoProvider = configurationElement.createExecutableExtension( "provider" );
        final IProfilPointPropertyProvider provider = (IProfilPointPropertyProvider) protoProvider;

        if( !THE_PROPERTY_PROVIDER_MAP.containsKey( profilType ) )
          THE_PROPERTY_PROVIDER_MAP.put( profilType, new ArrayList<IProfilPointPropertyProvider>() );

        THE_PROPERTY_PROVIDER_MAP.get( profilType ).add( provider );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return THE_PROPERTY_PROVIDER_MAP;
  }

  public static IProfilBuilder getProfilBuilder( final String type )
  {
    if( THE_PROFIL_BUILDER_MAP != null )
      return THE_PROFIL_BUILDER_MAP.get( type );

    THE_PROFIL_BUILDER_MAP = new HashMap<String, IProfilBuilder>();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] propertyProvider = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profilbuilder" );
    for( final IConfigurationElement configurationElement : propertyProvider )
    {
      try
      {
        // TODO: eventuell nur die elemente des gewünschten typs erzeugen und in map merken
        final String profilType = configurationElement.getAttribute( "profiletype" );
        final Object builderProvider = configurationElement.createExecutableExtension( "provider" );
        final IProfilBuilder provider = (IProfilBuilder) builderProvider;

        THE_PROFIL_BUILDER_MAP.put( profilType, provider );
      }
      catch( final CoreException e )
      {
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return THE_PROFIL_BUILDER_MAP.get( type );
  }

}