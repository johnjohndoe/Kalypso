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
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.UrlResolver;
import org.kalypso.util.progress.EclipseProgressMonitor;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 *  
 */
public class ShapeLoader extends AbstractLoader
{
  private final UrlResolver m_urlResolver = new UrlResolver();

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String,
   *      java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String location, final URL context,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      // eventuelle vorhandenen Information zum CRS abschneiden
      final int index = location.indexOf( '#' );

      final String sourceSrs;
      final String shpSource;
      if( index != -1 && index + 1 < location.length() )
      {
        sourceSrs = location.substring( index + 1 );

        shpSource = location.substring( 0, index );
      }
      else
      {
        sourceSrs = "EPSG:4326";
        shpSource = location;
      }

      final UrlResolver urlResolver = new UrlResolver();

      IResource shpResource = null;
      IResource dbfResource = null;
      IResource shxResource = null;

      final URL sourceURL = urlResolver.resolveURL( context, shpSource );

      final URL shpURL = urlResolver.resolveURL( context, shpSource + ".shp" );
      final URL dbfURL = urlResolver.resolveURL( context, shpSource + ".dbf" );
      final URL shxURL = urlResolver.resolveURL( context, shpSource + ".shx" );

      // leider können Shapes nicht aus URL geladen werden -> protocoll checken
      File sourceFile = null;
      final IPath resource = ResourceUtilities.findPathFromURL( sourceURL );
      if( resource != null )
      {
        sourceFile = ResourceUtilities.makeFileFromPath( resource );

        shpResource = ResourceUtilities.findFileFromURL( shpURL );
        dbfResource = ResourceUtilities.findFileFromURL( dbfURL );
        shxResource = ResourceUtilities.findFileFromURL( shxURL );
      }
      else
      {
        if( sourceURL.getProtocol().startsWith( "file:" ) )
          sourceFile = new File( sourceURL.getPath() );
      }

      if( sourceFile == null )
        throw new LoaderException( "Could not load shape at source: "
            + shpSource );

      // Workspace laden
      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      final CS_CoordinateSystem sourceCrs = org.kalypsodeegree_impl.model.cs.Adapters
          .getDefault().export( csFac.getCSByName( sourceSrs ) );

      final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault()
          .getCoordinatesSystem();
      final GMLWorkspace gmlWorkspace = ShapeSerializer.deserialize( sourceFile
          .getAbsolutePath(), sourceCrs, new EclipseProgressMonitor( monitor ) );
      final CommandableWorkspace workspace = new CommandableWorkspace(
          gmlWorkspace );

      try
      {
        workspace.accept( new TransformVisitor( targetCRS ), workspace
            .getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
        workspace.accept( new ResortVisitor(), workspace.getRootFeature(),
            FeatureVisitor.DEPTH_INFINITE );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }

      if( shpResource != null )
        addResource( shpResource, workspace );
      if( dbfResource != null )
        addResource( dbfResource, workspace );
      if( shxResource != null )
        addResource( shxResource, workspace );

      return workspace;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
  }

  public void save( final String source, final URL context,
      final IProgressMonitor monitor, final Object data )
      throws LoaderException
  {
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace)data;
      URL shpURL = m_urlResolver.resolveURL( context, source.split("#")[0] );

      final IFile file = ResourceUtilities.findFileFromURL( shpURL );
      if( file != null )
      {

        ShapeSerializer.serialize( workspace, file.getLocation().toFile().getAbsolutePath() );

      }
      else
        throw new LoaderException( "Die URL kann nicht beschrieben werden: "
            + shpURL );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      throw new LoaderException( "Der angegebene Pfad ist ungültig: " + source + "\n"
          + e.getLocalizedMessage(), e );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( "Fehler beim Speichern der URL\n" + e.getLocalizedMessage(), e );
    }
    
  }
}