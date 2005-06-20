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

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.UrlResolver;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Lädt einen GMLWorkspace aus einem GML
 * 
 * @author Belger
 */
public class GmlLoader extends AbstractLoader
{
  private final IUrlResolver m_urlResolver = new UrlResolver();

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String source, final URL context, final IProgressMonitor monitor )
      throws LoaderException
  {
    try
    {
      monitor.beginTask( "GML laden", 1000 );

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

      final CommandableWorkspace workspace = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( gmlURL,
          m_urlResolver ) );

      final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      workspace.accept( new ResortVisitor(), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );
      if( gmlFile != null )
        addResource( gmlFile, workspace );

      return workspace;
    }
    catch( final LoaderException le )
    {
      le.printStackTrace();

      throw le;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new LoaderException( "GML konnte nicht geladen werden: " + source, e );
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
    return "GML Layer";
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data )
      throws LoaderException
  {
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace)data;

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

      // ists im Workspace?
      final IFile file = ResourceUtilities.findFileFromURL( gmlURL );
      if( file != null )
      {
        final SetContentHelper thread = new SetContentHelper()
        {
          protected void write( final OutputStreamWriter writer ) throws Throwable
          {
            GmlSerializer.serializeWorkspace( writer, workspace );
          }
        };

        // damit der change event nicht kommt
        savingResource( file );

        thread.setFileContents( file, false, true, new NullProgressMonitor() );
      }
      else if( file == null && gmlURL.getProtocol().equals( "file" ) )
      {
        final OutputStreamWriter w = new FileWriter( new File( gmlURL.getFile() ) );
        GmlSerializer.serializeWorkspace( w, workspace );
      }
      else
        throw new LoaderException( "Die URL kann nicht beschrieben werden: " + gmlURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new LoaderException( "Der angegebene Pfad ist ungültig: " + source + "\n" + e.getLocalizedMessage(), e );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( "Fehler beim Speichern der URL\n" + e.getLocalizedMessage(), e );
    }
  }
}