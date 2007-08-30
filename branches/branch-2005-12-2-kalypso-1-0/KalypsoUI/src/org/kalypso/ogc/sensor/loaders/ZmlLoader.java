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
package org.kalypso.ogc.sensor.loaders;

import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;

/**
 * A specific loader for ZML-Files. Loads <code>ZmlObservation</code> objects.
 * 
 * @author schlienger
 */
public class ZmlLoader extends AbstractLoader
{
  private final UrlResolver m_urlResolver;

  public ZmlLoader()
  {
    m_urlResolver = new UrlResolver();
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final URL url = m_urlResolver.resolveURL( context, source );

      monitor.beginTask( "Zml laden aus: " + url, IProgressMonitor.UNKNOWN );

      final IObservation obs = ZmlFactory.parseXML( url, url.getFile() );

      // add resource in order to get aware of changes made by tier on it
      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file != null )
        addResource( file, obs );
      return obs;
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
//      e.printStackTrace();
      // TODO wenn resource geloescht wurde, wird hier ein fehler geworfen
      throw new LoaderException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  public void save( final String source, URL context, IProgressMonitor monitor, Object data ) throws LoaderException
  {
    try
    {
      if( data == null )
        return;
      final URL url = m_urlResolver.resolveURL( context, source );

      monitor.beginTask( "ZML speichern in: " + url, IProgressMonitor.UNKNOWN );

      final IFile file = ResourceUtilities.findFileFromURL( url );
      if( file == null )
        throw new IllegalArgumentException( "Datei konnte nicht gefunden werden: " + url );

      final ObservationType xmlObs = ZmlFactory.createXML( (IObservation)data, null );

      // set contents of ZML-file
      final SetContentHelper helper = new SetContentHelper()
      {
        protected void write( final OutputStreamWriter writer ) throws Throwable
        {
          final Marshaller marshaller = ZmlFactory.getMarshaller();
          marshaller.setProperty( Marshaller.JAXB_ENCODING, getCharset() );

          marshaller.marshal( xmlObs, writer );
        }
      };
      helper.setFileContents( file, false, true, new NullProgressMonitor() );
    }
    catch( Throwable e ) // generic exception caught for simplicity
    {
      throw new LoaderException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ZML";
  }
}