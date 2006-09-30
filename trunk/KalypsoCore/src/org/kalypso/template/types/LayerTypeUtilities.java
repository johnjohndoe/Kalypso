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

package org.kalypso.template.types;

import org.eclipse.core.internal.resources.PlatformURLResourceConnection;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * Utilities for the (xml-binding-)class {@link LayerType}. 
 *
 * @author bce
 */
@SuppressWarnings("restriction")
public class LayerTypeUtilities
{
  public static void initLayerType( final LayerType layer, final IFile file ) throws CoreException
  {
    final IPath projectRelativePath = file.getProjectRelativePath();

    final String fileext = projectRelativePath.getFileExtension();
    final String contentType;

    final String projectURL = PlatformURLResourceConnection.RESOURCE_URL_STRING + "/" + file.getProject().getName() + "/";
//   final String projectURL = "project:/";
    
    final String href;
    if( "gml".equalsIgnoreCase( fileext ) )
    {
      href = projectURL + projectRelativePath;
      contentType = "gml";
    }
    else if( "shp".equalsIgnoreCase( "shp" ) )
    {
      contentType = "shape";
      href = projectURL + projectRelativePath.removeFileExtension();
    }
    else
      throw new CoreException( StatusUtilities.createErrorStatus( "Unbekannte Dateiendung: " + fileext ) );

    layer.setId( file.getName() );
    layer.setFeaturePath( "" );
    layer.setHref( href );
    layer.setLinktype( contentType );
    layer.setType( "simple" );
  }
}
