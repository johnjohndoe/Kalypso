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
package org.kalypso.ogc.gml.loader;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.util.UrlResolver;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;

/**
 * @author schlienger
 *  
 */
public class SldLoader extends AbstractLoader
{
  private final UrlResolver m_urlResolver;

  public SldLoader()
  {
    m_urlResolver = new UrlResolver();

  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "OGC SLD";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String source, final URL context, final IProgressMonitor monitor )
      throws LoaderException
  {
    try
    {
      monitor.beginTask( "Lade SLD", 1000 );

      final URL url = m_urlResolver.resolveURL( context, source );

      final Reader reader = new InputStreamReader( url.openStream() );
      final StyledLayerDescriptor styledLayerDescriptor = SLDFactory.createSLD( reader );
      reader.close();

      final IResource resource = ResourceUtilities.findFileFromURL( url );
      addResource( resource, styledLayerDescriptor );

      monitor.done();
      return styledLayerDescriptor;
    }
    catch( final Exception e )
    {
      throw new LoaderException( e );
    }
  }

  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data )
  {
    System.out.print( "SLD-Loader Save method" );
  }
}