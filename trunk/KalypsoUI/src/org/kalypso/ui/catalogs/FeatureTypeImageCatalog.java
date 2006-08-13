/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.catalogs;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.catalog.CatalogManager;
import org.kalypso.core.catalog.ICatalog;
import org.kalypso.core.catalog.urn.IURNGenerator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author Gernot Belger
 */
public class FeatureTypeImageCatalog
{
  private static final String BASETYPE = "swtimage";

  public static ImageDescriptor getImage( final URL context, final QName qname )
  {
    final String urn = createImageUrn( qname );

    final String uri = getImageLocation( urn );
    // if we got no uri or an urn do nothing, we need a real url 
    if( uri == null || uri.startsWith( "urn" ))
      return null;

    try
    {
      final URL imgUrl = new URL( context, uri );
      return ImageDescriptor.createFromURL( imgUrl );
    }
    catch( final MalformedURLException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }

    return null;
  }

  public static String getImageLocation( final String urn )
  {
    // search for url
    final CatalogManager catalogManager = CatalogManager.getDefault();
    final ICatalog baseCatalog = catalogManager.getBaseCatalog();
    final String uri = baseCatalog.resolve( urn, urn );
    return uri;
  }

  public static String createImageUrn( final QName qname )
  {
    // REMARK: catalog is registered for feature type, not for qname
    // Hint for a refaktoring on the CatalogManager
    final IURNGenerator generator = CatalogManager.getDefault().getURNGeneratorFor( IFeatureType.class );
    if( generator == null )
      return null;

    final String baseURN = generator.generateURNFor( qname );
    if( baseURN == null )
      return null;

    return baseURN + ":" + BASETYPE + ":default";
  }

}
