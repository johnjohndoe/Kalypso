/*--------------- Kalypso-Header --------------------------------------------------------------------

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
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.loader;

import java.net.URL;

import org.eclipse.core.runtime.IProgressMonitor;

/**
 * 
 * 
 * @author schlienger
 */
public interface ILoader
{
  public String getDescription();
  
  /**
   * Loads an object from somewhere.
   * 
   * @param location information about the location of the resource to load
   * @param context some context for making the relative location of the resource to load absolute
   * @param monitor monitors the progress of loading
   * @return object
   * @throws LoaderException
   */
  public Object load( final String location, final URL context, final IProgressMonitor monitor ) throws LoaderException;
  
  public void save( final String location, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException;

  /**
   * TODO: document this
   * @param object
   */
  public void release( final Object object );
  
  public void addLoaderListener( final ILoaderListener l );
  
  public void removeLoaderListener( final ILoaderListener l );
}
