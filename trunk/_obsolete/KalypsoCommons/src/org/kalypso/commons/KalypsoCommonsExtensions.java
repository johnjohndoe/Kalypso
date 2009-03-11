/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.commons;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.vfs.FileObject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.i18n.ITranslator;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.IProcessFactory;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * Extensions of <code>org.kalypso.commons</code>.
 * 
 * @author Gernot Belger
 */
public class KalypsoCommonsExtensions
{
  private final static String I10N_TRANSLATOR_EXTENSION_POINT = "org.kalypso.commons.i18n";

  private final static String PROCESS_EXTENSION_POINT = "org.kalypso.commons.process";

  private static Map<String, IConfigurationElement> m_i10nExtensions;

  private static Map<String, IConfigurationElement> m_processExtensions;

  public static synchronized ITranslator createTranslator( final String id )
  {
    if( id == null )
      return null;

    if( m_i10nExtensions == null )
    {
      m_i10nExtensions = new HashMap<String, IConfigurationElement>();

      final IExtensionRegistry registry = Platform.getExtensionRegistry();

      final IExtensionPoint extensionPoint = registry.getExtensionPoint( I10N_TRANSLATOR_EXTENSION_POINT );

      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

      for( final IConfigurationElement element : configurationElements )
      {
        final String elementId = element.getAttribute( "id" );
        m_i10nExtensions.put( elementId, element );
      }
    }

    final KalypsoCommonsPlugin activator = KalypsoCommonsPlugin.getDefault();
    final IConfigurationElement element = m_i10nExtensions.get( id );
    if( element == null )
    {
      final Status status = new Status( IStatus.ERROR, activator.getBundle().getSymbolicName(), 1, "No i10nTranslator with id: " + id, null );
      activator.getLog().log( status );
      return null;
    }

    try
    {
      return (ITranslator) element.createExecutableExtension( "class" );
    }
    catch( final CoreException e )
    {
      activator.getLog().log( e.getStatus() );
      return null;
    }
  }

  /**
   * @param factoryId
   *          The extension-id of the {@link IProcessFactory} that should be used in order to create the new process.
   * @see IProcessFactory#newProcess(File, String, String[])
   */
  public static synchronized IProcess createProcess( final String factoryId, final FileObject workingDir, final URL executeable, final String[] commandlineArgs ) throws CoreException
  {
    Assert.isNotNull( factoryId );

    final IProcessFactory factory = getProcessFactory( factoryId );
    if( factory == null )
    {
      final Status status = new Status( IStatus.ERROR, KalypsoCommonsPlugin.getID(), 1, "No process factory with id: " + factoryId, null );
      throw new CoreException( status );
    }

    try
    {
      return factory.newProcess( workingDir, executeable, commandlineArgs );
    }
    catch( final IOException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e, "Problem creating process for executable %s in working dir %s.", executeable.toString(), workingDir.toString() );
      throw new CoreException( status );
    }
  }

  private static IProcessFactory getProcessFactory( final String id ) throws CoreException
  {
    synchronized( PROCESS_EXTENSION_POINT )
    {
      if( m_processExtensions == null )
      {
        m_processExtensions = new HashMap<String, IConfigurationElement>();

        final IExtensionRegistry registry = Platform.getExtensionRegistry();

        final IExtensionPoint extensionPoint = registry.getExtensionPoint( PROCESS_EXTENSION_POINT );

        final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();

        for( final IConfigurationElement element : configurationElements )
        {
          final String elementId = element.getAttribute( "id" );
          m_processExtensions.put( elementId, element );
        }
      }
    }

    final IConfigurationElement element = m_processExtensions.get( id );
    if( element == null )
      return null;

    return (IProcessFactory) element.createExecutableExtension( "class" );
  }
}
