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
package org.kalypso.commons.bind;

import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;
import javax.xml.validation.Schema;
import javax.xml.validation.Validator;

import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.commons.xml.NSPrefixProvider;
import org.kalypso.contribs.eclipse.core.runtime.Debug;
import org.kalypso.contribs.eclipse.core.runtime.DebugPerf;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * @author Gernot Belger
 */
public final class JaxbUtilities
{
  private static final String COM_SUN_XML_BIND_NAMESPACE_PREFIX_MAPPER = "com.sun.xml.bind.namespacePrefixMapper";

  public final static Debug DEBUG = new Debug( KalypsoCommonsPlugin.getDefault(), "org.kalypso.jwsdp", "/perf/jaxbinitialisation", System.out );

  private JaxbUtilities( )
  {
    // never instantiate
  }

  /**
   * Creates a {@link JAXBContext} for the given object factories.<br>
   * This is a convienience method in order to avoid handling of the thrown {@link JAXBException} which will never come
   * during normal execution.<br>
   * Additionally, it is vital to use this method, as it enforces the use of the right JAXB Version even if called from
   * an ant task (see comment in code).
   */
  public static JAXBContext createQuiet( final Class< ? >... objectFactoryClasses )
  {
    final DebugPerf stopWatch = DEBUG.createStopWatch( "Initializing JAXContext for classes:%n" );
    for( final Class< ? > clazz : objectFactoryClasses )
      DEBUG.printf( "\t%s%n", clazz );

    final Thread currentThread = Thread.currentThread();
    final ClassLoader contextClassLoader = currentThread.getContextClassLoader();
    try
    {
      // REMARK/HACK: this is a crude hack to enforce the right class loader if invoked from an Ant-Task.
      // If we do not do this, the default JAXBContext (com.sun.xml.internal.bind.v2.runtime.JAXBContextImpl.class)
      // will be used instead of JAXB-RI (currently 2.1.7 in the KalypsoCommons plug-in).
      // Setting the System-Property 'javax.xml.bind.JAXBContext' does not help either,
      // as the right ContextFactory is not accessible via the AntClassLoader.
      // Maybe there is another, more clean way to do this?
      if( contextClassLoader.getClass().getName().equals( "org.eclipse.ant.internal.core.AntClassLoader" ) )
        currentThread.setContextClassLoader( JaxbUtilities.class.getClassLoader() );

      return JAXBContext.newInstance( objectFactoryClasses );
    }
    catch( final JAXBException e )
    {
      // Always print stack trace, this should not happen...
      e.printStackTrace();
      return null;
    }
    finally
    {
      // Always restore old context class loader, who knows what happens else...
      currentThread.setContextClassLoader( contextClassLoader );

      stopWatch.stopWatch( "JAXContext created.%n%n" );
    }
  }

  private static NamespacePrefixMapper getNSPrefixMapper( )
  {
    return new NamespacePrefixMapper()
    {
      @Override
      public String getPreferredPrefix( final String namespace, final String suggestion, final boolean required )
      {
        // never return null to avoid using of defaultnamespace witch leads to broken xml sometimes. see bug-description
        // at methode createMarshaller()
        final NSPrefixProvider nsProvider = NSPrefixProvider.getInstance();
        return nsProvider.getPreferredPrefix( namespace, suggestion );
      }
    };
  }

  private static NamespacePrefixMapper getNSPrefixMapper( final Map<String, String> specialPrefixMapping )
  {
    return new NamespacePrefixMapper()
    {
      @Override
      public String getPreferredPrefix( final String namespace, final String suggestion, final boolean required )
      {
        if( specialPrefixMapping.containsKey( namespace ) )
          return specialPrefixMapping.get( namespace );

        // never return null to avoid using of defaultnamespace witch leads to broken xml sometimes. see bug-description
        // at methode createMarshaller()
        final NSPrefixProvider nsProvider = NSPrefixProvider.getInstance();
        return nsProvider.getPreferredPrefix( namespace, suggestion );
      }
    };
  }

  /**
   * Same as {@link #createMarshaller(JAXBContext, true, null)}
   */
  public static Marshaller createMarshaller( final JAXBContext context ) throws JAXBException
  {
    return createMarshaller( context, false );
  }

  /**
   * Same as {@link #createMarshaller(JAXBContext, boolean, null)}
   */
  public static Marshaller createMarshaller( final JAXBContext context, final boolean formatOutput ) throws JAXBException
  {
    return createMarshaller( context, formatOutput, null );
  }

  /**
   * Create marshaller with namespace prefixmapping<br>
   * use this methode to avoid bug in jaxb-implementation when prefixing attributes with defaultnamespace like this:
   * <code><element xmlns="www.xlink..." :href="test"/></code> instead if
   * <code><element xmlns:xlink="blabla" xlink:href="test"/></code>.
   * 
   * @param formaOutput
   *          if true, output is nicely formatted.
   * @param specialPrefixes
   *          if non null, use these mappings before the usual prefix mappings. Use with care, it is not tested if
   *          namespaces are mapped into the same namespace.
   */
  public static Marshaller createMarshaller( final JAXBContext context, final boolean formatOutput, final Map<String, String> specialPrefixes ) throws JAXBException
  {
    final Marshaller marshaller = context.createMarshaller();
    if( specialPrefixes == null )
      marshaller.setProperty( COM_SUN_XML_BIND_NAMESPACE_PREFIX_MAPPER, getNSPrefixMapper() );
    else
      marshaller.setProperty( COM_SUN_XML_BIND_NAMESPACE_PREFIX_MAPPER, getNSPrefixMapper( specialPrefixes ) );
    if( formatOutput )
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    return marshaller;
  }

  public static Validator createValidator( final JAXBContext context ) throws JAXBException
  {
    final Unmarshaller unmarshaller = context.createUnmarshaller();
    final Schema schema = unmarshaller.getSchema();
    if( schema == null )
      return null;

    return schema.newValidator();
  }

  @SuppressWarnings("unchecked")
  public static JAXBElement<Object> createJaxbElement( final QName qname, final Object value )
  {
    return new JAXBElement<Object>( qname, (Class<Object>) value.getClass(), value );
  }

}
