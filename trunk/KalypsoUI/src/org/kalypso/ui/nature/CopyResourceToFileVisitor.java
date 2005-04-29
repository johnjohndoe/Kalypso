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
package org.kalypso.ui.nature;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;

import org.apache.commons.io.CopyUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * TODO an JH: bist Du sicher, dass diese Klasse überhauipt noch benutzt wird?
 * Ich glaube nämlich seit kurzem nicht mehr! Gernot
 * 
 * @author belger
 */
public class CopyResourceToFileVisitor implements IResourceVisitor
{
  private final IContainer m_baseresource;

  private final File m_targetDir;

  public CopyResourceToFileVisitor( final IContainer baseresource, final File targetDir )
  {
    m_baseresource = baseresource;
    m_targetDir = targetDir;
  }

  /**
   * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
   */
  public boolean visit( final IResource resource ) throws CoreException
  {
    String suffix = resource.getFileExtension();
    // nur Dateien werden kopiert
    if( resource instanceof IFile )
    {
      final IFile inputfile = (IFile)resource;
      final IPath inputpath = resource.getFullPath();

      // Error msg?
      final IPath basePath = m_baseresource.getFullPath();
      if( !basePath.isPrefixOf( inputpath ) )
        return true;

      final String basepath = basePath.toString();
      final String resourcepath = inputpath.toString();

      final String relativepath = resourcepath.substring( basepath.length() + 1 );

      final File targetpath = new File( m_targetDir, relativepath );
      targetpath.getParentFile().mkdirs();

      // daten kopieren
      //TODO: JH, unschöner hack wegen zip zukünftig in modelspec encoding angeben und binaries entsprechend als stream kopieren
      final String zipSuffix = new String( "zip" );
      if( suffix.equals( zipSuffix ) )
      {
        try
        {
          System.out.println( "Datei ist ZIP" );
          final FileOutputStream outstream = new FileOutputStream( targetpath );
          final InputStream inputstream = inputfile.getContents();
//          final InputStream inputstream = new InputStream(inputfile.getContents());
          CopyUtils.copy( inputstream, outstream );
          
          // TODO an JH: das ist nicht sicher! wenns ne exception gibt, werden die Streams nicht
          // geschlossen! Besser immer in einem finally-block mit IOUtils.closeQuietly() schlieesen
          inputstream.close();
          outstream.close();

        }
        catch( final Exception e )
        {
          e.printStackTrace();
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Fehler beim Kopieren einer Datei", e ) );
        }
      }
      else
      {
        try
        {
          final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream(
              targetpath ), inputfile.getCharset() );
          final Reader r = new InputStreamReader( inputfile.getContents(), inputfile.getCharset() );

          // bean erzeugen
          CopyUtils.copy( r, writer );
          r.close();
          writer.close();
        }
        catch( final CoreException e )
        {
          throw e;
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Fehler beim Kopieren einer Datei", e ) );
        }
      }
    }

    return true;
  }

}