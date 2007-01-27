package org.kalypso.model.wspm.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.model.wspm.core.gml.IProfileFeatureProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;
import org.kalypso.model.wspm.core.profil.impl.marker.ProfilPointMarker;
import org.kalypso.model.wspm.core.profil.reparator.IProfilReparator;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;

/** Helper class to read extension points of this plugin. */
public class KalypsoModelWspmCoreExtensions
{
  private static IProfileFeatureProvider[] PROFILE_FEATURE_PROVIDER = null;

  private static IProfilePointFilter[] PROFILE_POINT_FILTER = null;

  private static Map<String, IProfilPointMarker> PROFILE_POINT_MARKER = null;

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
   * @param fileExtension File extension without '.'
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
    for( int i = 0; i < elements.length; i++ )
    {
      final IConfigurationElement element = elements[i];
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
    for( int i = 0; i < elements.length; i++ )
    {
      final IConfigurationElement element = elements[i];
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

  public static Map<String, IProfilPointMarker> getProfilePointMarker( )
  {
    if( PROFILE_POINT_MARKER != null )
      return PROFILE_POINT_MARKER;
    final IExtensionRegistry registry = Platform.getExtensionRegistry();
    final IConfigurationElement[] markers = registry.getConfigurationElementsFor( "org.kalypso.model.wspm.core.profilPointMarker" );
    PROFILE_POINT_MARKER = new HashMap<String, IProfilPointMarker>( markers.length );
    for( int i = 0; i < markers.length; i++ )
    {
      final IConfigurationElement marker = markers[i];

      final IConfigurationElement[] markerParam = marker.getChildren( "parameter" );
      final String[] params = new String[markerParam.length];
      for( int k = 0; k < markerParam.length; k++ )
      {
        params[k] = markerParam[k].getAttribute( "key" );
      }
      PROFILE_POINT_MARKER.put( marker.getAttribute( "id" ), new ProfilPointMarker( marker.getAttribute( "id" ), params ) );
    }
    return PROFILE_POINT_MARKER;
  }
}
