/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 * 
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */

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
import org.kalypso.contribs.eclipse.core.runtime.ExtensionUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.filterencoding.IFunctionExpression;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.validation.IFeatureRule;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.GmlWorkspaceListener;

/**
 * Helper class to read extension-points of this plugin.
 * 
 * @author Gernot Belger
 */
public class KalypsoDeegreeExtensions
{
  /* Empty implementation which does nothing. */
  private static final IGmlWorkspaceListener INVALID_PROXY = new GmlWorkspaceListener()
  {
    public void init( final GMLWorkspace workspace )
    {
    }

    public void onModellChange( final ModellEvent modellEvent )
    {
    }
  };

  private final static String FUNCTION_EXTENSION_POINT = "org.kalypso.deegree.functionProperty";

  private final static String LISTENER_EXTENSION_POINT = "org.kalypso.deegree.gmlWorkspaceListener";

  private final static String RULES_EXTENSION_POINT = "org.kalypso.deegree.featureRule";

  private final static String FEATUREBINDING_EXTENSION_POINT = "org.kalypso.deegree.featureBinding";

  private static final IGmlWorkspaceListener[] EMPTY_LISTENERS = new IGmlWorkspaceListener[] {};

  private static Map<QName, IConfigurationElement> FEATURE_BINDINGS = null;

  private static Map<String, IConfigurationElement> FUNCTION_EXPRESSION = null;

  private static Map<QName, List<IFeatureRule>> THE_RULES = null;

  private static Map<QName, IConfigurationElement[]> QNAME_LISTENERS = null;

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

  private static synchronized Map<QName, IConfigurationElement[]> createQNameListeners( )
  {
    if( QNAME_LISTENERS == null )
    {
      final IExtensionRegistry registry = Platform.getExtensionRegistry();

      final IExtensionPoint extensionPoint = registry.getExtensionPoint( LISTENER_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      final Map<QName, List<IConfigurationElement>> listeners = new HashMap<QName, List<IConfigurationElement>>( configurationElements.length );
      for( final IConfigurationElement element : configurationElements )
      {
        final QName[] qnames = parseQNames( element );
        if( qnames.length == 0 )
          addQName( listeners, element, null );
        else
        {
          for( final QName qname : qnames )
            addQName( listeners, element, qname );
        }
      }

      /* Make arrays from list */
      QNAME_LISTENERS = new HashMap<QName, IConfigurationElement[]>( listeners.size() );
      for( final Map.Entry<QName, List<IConfigurationElement>> entry : listeners.entrySet() )
      {
        final List<IConfigurationElement> value = entry.getValue();
        final IConfigurationElement[] workspaceListeners = value.toArray( new IConfigurationElement[value.size()] );

        QNAME_LISTENERS.put( entry.getKey(), workspaceListeners );
      }
    }

    return QNAME_LISTENERS;
  }

  public static QName[] parseQNames( final IConfigurationElement element )
  {
    final IConfigurationElement[] children = element.getChildren( "qname" );

    final List<QName> qnames = new ArrayList<QName>( children.length );
    for( final IConfigurationElement child : children )
    {
      final String namespace = child.getAttribute( "namespace" );
      final String localPart = child.getAttribute( "localPart" );

      qnames.add( new QName( namespace, localPart ) );
    }

    return qnames.toArray( new QName[qnames.size()] );
  }

  /**
   * Helper method to add a new entry into the listener-lists
   */
  private static void addQName( final Map<QName, List<IConfigurationElement>> listeners, final IConfigurationElement element, final QName qname )
  {
    if( !listeners.containsKey( qname ) )
      listeners.put( qname, new ArrayList<IConfigurationElement>() );

    final List<IConfigurationElement> list = listeners.get( qname );
    list.add( element );
  }

  /**
   * Get all listeners which are associated with the given qname.
   * 
   * @param qname
   *          If null, the listeners are returned which are not associated with any qname.
   */
  public static IGmlWorkspaceListener[] createGmlWorkspaceListeners( final QName qname )
  {
    final Map<QName, IConfigurationElement[]> nameListeners = createQNameListeners();
    final IConfigurationElement[] elements = nameListeners.get( qname );
    if( elements == null )
      return EMPTY_LISTENERS;

    final IGmlWorkspaceListener[] listeners = new IGmlWorkspaceListener[elements.length];
    for( int i = 0; i < elements.length; i++ )
    {
      try
      {
        listeners[i] = (IGmlWorkspaceListener) elements[i].createExecutableExtension( "class" );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e, "Failed to instantiate workspace listener: " + elements[i].getAttribute( "id" ) );
        KalypsoDeegreePlugin.getDefault().getLog().log( status );

        listeners[i] = INVALID_PROXY;
      }
    }

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

  /**
   * @return list of feature binding handlers, handling a special featureType qname
   */
  public synchronized static IConfigurationElement getFeatureBinding( final QName qname )
  {
    // fill binding map
    if( FEATURE_BINDINGS == null )
    {
      FEATURE_BINDINGS = new HashMap<QName, IConfigurationElement>();

      /* get extension points */
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IConfigurationElement[] elements = registry.getConfigurationElementsFor( FEATUREBINDING_EXTENSION_POINT );

      for( final IConfigurationElement configurationElement : elements )
      {
        final String qnameAttrribute = configurationElement.getAttribute( "qname" );
        final QName qn = QName.valueOf( qnameAttrribute );

        FEATURE_BINDINGS.put( qn, configurationElement );
      }
    }

    return FEATURE_BINDINGS.get( qname );
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

  public static IFunctionExpression createFunctionExpression( final String name ) throws CoreException
  {
    final Map<String, IConfigurationElement> functions = getFunctionExpressionElements();
    final IConfigurationElement configurationElement = functions.get( name );
    return (IFunctionExpression) configurationElement.createExecutableExtension( "class" );
  }

  /**
   * Returns the registered extension of the functionExpression extenion-point, indexed by its name attribute.
   */
  private static synchronized Map<String, IConfigurationElement> getFunctionExpressionElements( )
  {
    if( FUNCTION_EXPRESSION == null )
      FUNCTION_EXPRESSION = ExtensionUtilities.getConfigurationElements( "org.kalypso.deegree.functionExpression", "name" );

    return FUNCTION_EXPRESSION;
  }

}
