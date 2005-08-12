/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso;

import java.io.File;
import java.io.IOException;

import javax.xml.bind.JAXBException;

import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.lhwsachsenanhalt.tubig.TubigUrlCatalog;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class KalypsoTest
{
  private static boolean init = false;

  public static void init() throws IOException, TypeRegistryException, JAXBException
  {
    if( init )
      return;
    init = true;
    ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );
    registry.registerTypeHandler( new DiagramTypeHandler() );
    registry.registerTypeHandler( new RangeSetTypeHandler() );
    registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );

    final IUrlCatalog theCatalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new DeegreeUrlCatalog(),
        new UrlCatalogUpdateObservationMapping(),
        //        new UrlCatalogUpdateObservationMapping(),
        new TubigUrlCatalog(),
        new UrlCatalogNA() } );

    final File tmp = File.createTempFile( "kalypsotest", ".cache" );
    final File cacheDir = new File( tmp.getParentFile(), "schemaCache" );
    cacheDir.mkdir();

    GMLSchemaCatalog.init( theCatalog, cacheDir );

  }
}
