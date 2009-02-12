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
package org.kalypso.workflow;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.kalypso.contribs.java.net.IUrlResolver2;

/**
 * multi context for workflow<br>
 * <li>context for active project </li>
 * <li>context for active calccase </li>
 * <li>context for actionURL</li>
 * 
 * @author doemming
 */
public class WorkflowContext implements IUrlResolver2
{
  private IProject m_contextProject = null;

  private IFolder m_contextCalcDir = null;

  private URL m_contextActionURL = null;

  private URL m_contextHome = null;

  public void setContextProject( IProject contextProject )
  {
    m_contextProject = contextProject;
  }

  public IProject getContextProject( )
  {
    return m_contextProject;
  }

  public void setContextCalcDir( IFolder contextCalcDir )
  {
    m_contextCalcDir = contextCalcDir;
  }

  public IFolder getContextCalcDir( )
  {
    return m_contextCalcDir;
  }

  public void setContextActionURL( final URL contextActionURL )
  {
    m_contextActionURL = contextActionURL;
    System.out.println( "context for actionURL: " + m_contextActionURL );
  }

  public URL getContextActionURL( )
  {
    return m_contextActionURL;
  }

  /**
   * @see org.kalypso.contribs.java.net.IUrlResolver2#resolveURL(java.lang.String)
   */
  public URL resolveURL( final String relativeOrAbsolute ) throws MalformedURLException
  {
    if( relativeOrAbsolute == null ) // nothing to resolve
      return getContextActionURL(); // return base

    final IProject contextProject = getContextProject();
    final IFolder contextCalcDir = getContextCalcDir();
    final HashMap<String, String> replaceTokens = new HashMap<String, String>();
    if( contextProject != null )
      replaceTokens.put( "project:", "platform:/resource/" + contextProject.getName() + "/" );
    if( contextCalcDir != null )
      replaceTokens.put( "calcdir:", "platform:/resource/" + contextCalcDir.getFullPath().toString() + "/" );
    final Set<Entry<String, String>> entrySet = replaceTokens.entrySet();
    final Iterator<Entry<String, String>> iterator = entrySet.iterator();
    String relative = relativeOrAbsolute;
    while( iterator.hasNext() )
    {
      final Entry<String, String> entry = iterator.next();
      relative = relative.replaceFirst( entry.getKey(), entry.getValue() );
    }
    return new URL( getContextActionURL(), relative );
  }

  public URL getContextHome( )
  {
    return m_contextHome;
  }

  public void setContextHome( URL contextHome )
  {
    m_contextHome = contextHome;
  }

}
