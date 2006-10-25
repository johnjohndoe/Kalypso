/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.ui.profil.validation;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.ui.IMarkerResolutionGenerator2;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.osgi.framework.Bundle;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

/**
 * @author kimwerner
 */
public class ProfilMarkerResolutionGenerator implements IMarkerResolutionGenerator2
{
  private XStream m_xstream;

  public ProfilMarkerResolutionGenerator( )
  {
    m_xstream = new XStream( new DomDriver() );
  }

  /**
   * @see org.eclipse.ui.IMarkerResolutionGenerator2#hasResolutions(org.eclipse.core.resources.IMarker)
   */
  public boolean hasResolutions( final IMarker marker )
  {
    final IMarkerResolution2[] mss = getResolutions( marker );
    return mss != null && mss.length > 0;
  }

  /**
   * @see org.eclipse.ui.IMarkerResolutionGenerator#getResolutions(org.eclipse.core.resources.IMarker)
   */
  public IMarkerResolution2[] getResolutions( final IMarker marker )
  {
    final String pluginId = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_PLUGINID, (String) null );

    final Bundle bundle = Platform.getBundle( pluginId );

    final ClassLoader bundleLoader = new ClassLoader()
    {
      /**
       * @see java.lang.ClassLoader#loadClass(java.lang.String)
       */
      @Override
      public Class< ? > loadClass( final String name ) throws ClassNotFoundException
      {
        return bundle.loadClass( name );
      }
    };

    final String resolutions = marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS, (String) null );

    m_xstream.setClassLoader( bundleLoader );
    final IMarkerResolution2[] mss = (IMarkerResolution2[]) m_xstream.fromXML( resolutions );
    // reset class-loader in order to free the inner class
    m_xstream.setClassLoader( this.getClass().getClassLoader() );
    
    return mss;
  }
}
