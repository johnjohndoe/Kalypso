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

package org.kalypso.metadoc.impl;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.metadoc.IExportTarget;

/**
 * Abstract export target, which handles the common extension point stuff.
 * 
 * @author schlienger
 */
public abstract class AbstractExportTarget implements IExportTarget
{
  private String m_name;

  private String m_desc;

  private ImageDescriptor m_imageDescriptor;

  /** contains the list of modes. If it is empty, all modes are supported */
  private final Set<String> m_modes = new HashSet<String>();

  /** holder for the properties */
  private final Configuration m_properties = new BaseConfiguration();

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public final void setInitializationData( final IConfigurationElement config, final String propertyName, Object data )
  {
    m_name = config.getAttribute( "name" );
    m_desc = config.getAttribute( "description" );

    final String modes = config.getAttribute( "modes" );
    if( modes != null && modes.length() > 0 )
      m_modes.addAll( Arrays.asList( modes.split( ";" ) ) );

    final String iconLocation = config.getAttribute( "icon" );
    if( iconLocation != null )
      m_imageDescriptor = AbstractUIPlugin.imageDescriptorFromPlugin( config.getDeclaringExtension().getContributor().getName(), iconLocation );
  }

  /**
   * @see org.kalypso.metadoc.IExportTarget#getName()
   */
  public final String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.metadoc.IExportTarget#getDescription()
   */
  public final String getDescription( )
  {
    return m_desc;
  }

  /**
   * @see org.kalypso.metadoc.IExportTarget#getImage()
   */
  public final ImageDescriptor getImage( )
  {
    return m_imageDescriptor;
  }

  /**
   * @see org.kalypso.metadoc.IExportTarget#isModeSupported(java.lang.String)
   */
  public boolean isModeSupported( final String mode )
  {
    return mode == null || m_modes.isEmpty() || m_modes.contains( mode );
  }

  /**
   * @see org.kalypso.metadoc.IExportTarget#setProperty(java.lang.String, java.lang.String)
   */
  public void setProperty( final String key, final String value )
  {
    m_properties.setProperty( key, value );
  }

  /**
   * @return the properties of this target. Should only be used internally, or by subclasses.
   */
  protected Configuration getProperties( )
  {
    return m_properties;
  }
}
