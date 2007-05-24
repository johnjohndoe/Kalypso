/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypsodeegree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GmlWorkspaceListenerProxy;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.validation.IFeatureRule;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Helper class to read extension-points of this plugin.
 * 
 * @author Gernot Belger
 */
public class KalypsoDeegreeExtensions
{
  private final static String FUNCTION_EXTENSION_POINT = "org.kalypso.deegree.functionProperty";

  private final static String LISTENER_EXTENSION_POINT = "org.kalypso.deegree.gmlWorkspaceListener";

  private final static String RULES_EXTENSION_POINT = "org.kalypso.deegree.featureRule";

  private static final IGmlWorkspaceListener[] EMPTY_LISTENERS = new IGmlWorkspaceListener[] {};

  private static Map<QName, List<IFeatureRule>> THE_RULES = null;

  private static Map<QName, IGmlWorkspaceListener[]> QNAME_LISTENERS = null;

  private static Map<String, IConfigurationElement> FUNCTION_MAP = null;

  public static synchronized FeaturePropertyFunction createPropertyFunction( final String id, final Map<String, String> properties ) throws CoreException
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( FUNCTION_EXTENSION_POINT );
    if( FUNCTION_MAP == null )
    {
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      FUNCTION_MAP = new HashMap<String, IConfigurationElement>( configurationElements.length );
      for( final IConfigurationElement element : configurationElements )
      {
        final String configid = element.getAttribute( "id" );
        FUNCTION_MAP.put( configid, element );
      }
    }

    if( !FUNCTION_MAP.containsKey( id ) )
    {
      final IStatus status = StatusUtilities.createErrorStatus( "No function property with id: " + id );
      throw new CoreException( status );
    }

    final IConfigurationElement element = FUNCTION_MAP.get( id );
    final FeaturePropertyFunction function = (FeaturePropertyFunction) element.createExecutableExtension( "class" );
    function.init( properties );

    return function;
  }

  private static synchronized Map<QName, IGmlWorkspaceListener[]> getQNameListeners( )
  {
    if( QNAME_LISTENERS == null )
    {
      final IExtensionRegistry registry = Platform.getExtensionRegistry();

      final IExtensionPoint extensionPoint = registry.getExtensionPoint( LISTENER_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      final Map<QName, List<IGmlWorkspaceListener>> listeners = new HashMap<QName, List<IGmlWorkspaceListener>>( configurationElements.length );
      for( final IConfigurationElement element : configurationElements )
      {
        final IGmlWorkspaceListener listener = new GmlWorkspaceListenerProxy( element );
        final QName[] qnames = listener.getQNames();
        if( qnames.length == 0 )
          addQName( listeners, listener, null );
        else
        {
          for( final QName qname : qnames )
            addQName( listeners, listener, qname );
        }
      }

      /* Make arrays from list */
      QNAME_LISTENERS = new HashMap<QName, IGmlWorkspaceListener[]>( listeners.size() );
      for( final Map.Entry<QName, List<IGmlWorkspaceListener>> entry : listeners.entrySet() )
      {
        final List<IGmlWorkspaceListener> value = entry.getValue();
        final IGmlWorkspaceListener[] workspaceListeners = value.toArray( new IGmlWorkspaceListener[value.size()] );

        QNAME_LISTENERS.put( entry.getKey(), workspaceListeners );
      }
    }

    return QNAME_LISTENERS;
  }

  /**
   * Helper method to add a new entry into the listener-lists
   */
  private static void addQName( final Map<QName, List<IGmlWorkspaceListener>> listeners, final IGmlWorkspaceListener listener, final QName qname )
  {
    if( !listeners.containsKey( qname ) )
      listeners.put( qname, new ArrayList<IGmlWorkspaceListener>() );

    final List<IGmlWorkspaceListener> list = listeners.get( qname );
    list.add( listener );
  }

  /**
   * Get all listeners which are associated with the given qname.
   * 
   * @param qname
   *            If null, the listeners are returned which are not associated with any qname.
   */
  public static IGmlWorkspaceListener[] getGmlWorkspaceListeners( final QName qname )
  {
    final Map<QName, IGmlWorkspaceListener[]> nameListeners = getQNameListeners();
    final IGmlWorkspaceListener[] listeners = nameListeners.get( qname );
    if( listeners == null )
      return EMPTY_LISTENERS;

    return listeners;
  }

  public static IFeatureRule[] getFeatureRules( final Feature feature )
  {
    final Map<QName, List<IFeatureRule>> rules = getFeatureRules();

    final IFeatureType featureType = feature.getFeatureType();
    final IFeatureType[] substituteFeatureTypes = GMLSchemaUtilities.getSubstituts( featureType, featureType.getGMLSchema(), true, true );

    final Set<IFeatureRule> result = new HashSet<IFeatureRule>();
    for( final IFeatureType type : substituteFeatureTypes )
    {
      final List<IFeatureRule> list = rules.get( type );
      if( list != null )
        result.addAll( list );
    }

    final List<IFeatureRule> commonRules = rules.get( null );
    if( commonRules != null )
      result.addAll( commonRules );

    return result.toArray( new IFeatureRule[result.size()] );
  }

  private synchronized static Map<QName, List<IFeatureRule>> getFeatureRules( )
  {
    if( THE_RULES != null )
      return THE_RULES;

    THE_RULES = new HashMap<QName, List<IFeatureRule>>();

    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    final IExtensionPoint extensionPoint = registry.getExtensionPoint( RULES_EXTENSION_POINT );
    final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
    for( final IConfigurationElement element : configurationElements )
    {
      try
      {
        final IFeatureRule rule = (IFeatureRule) element.createExecutableExtension( "class" );
        final QName[] names = rule.getQNames();
        if( names == null )
          addRule( rule, null );
        else
        {
          for( final QName name : names )
            addRule( rule, name );
        }
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        e.printStackTrace();
        KalypsoDeegreePlugin.getDefault().getLog().log( status );
      }
    }

    return THE_RULES;
  }

  private static void addRule( final IFeatureRule rule, final QName name )
  {
    if( !THE_RULES.containsKey( name ) )
      THE_RULES.put( name, new ArrayList<IFeatureRule>() );

    final List<IFeatureRule> nameList = THE_RULES.get( name );
    nameList.add( rule );
  }

}
