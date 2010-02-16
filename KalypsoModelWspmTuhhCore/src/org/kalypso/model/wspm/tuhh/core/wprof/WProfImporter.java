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
package org.kalypso.model.wspm.tuhh.core.wprof;

import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class WProfImporter
{
  private final Collection<IWProfContentHandler> m_handlers = new ArrayList<IWProfContentHandler>();

  private final String m_shapePath;

  private String m_shapeDefaultSrs;

  private final IWProfPointFactory m_pointFactory;

  public WProfImporter( final String shapePath, final IWProfPointFactory pointFactory )
  {
    m_shapePath = shapePath;
    m_pointFactory = pointFactory;
  }

  public void addHandler( final IWProfContentHandler handler )
  {
    m_handlers.add( handler );
  }

  public void setShapeDefaultSrs( final String srs )
  {
    m_shapeDefaultSrs = srs;
  }

  public void importW80Shape( final IProgressMonitor monitor ) throws GmlSerializeException, CoreException, MalformedURLException
  {
    monitor.beginTask( String.format( "Importing %s", m_shapePath ), 1000 );

    /* Load Shape */
    final File prjFile = new File( m_shapePath + ".prj" );
    final String shapeSrs = ShapeSerializer.loadCrs( prjFile.toURI().toURL(), m_shapeDefaultSrs );

    monitor.subTask( "reading shape file..." );
    final GMLWorkspace w80shapeWorkspace = ShapeSerializer.deserialize( m_shapePath, shapeSrs, new SubProgressMonitor( monitor, 500 ) );

    final FeatureList w80features = (FeatureList) w80shapeWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    System.out.println( String.format( "Read %d points", w80features.size() ) );

    /* Data */
    monitor.subTask( "converting data..." );
    importW80Data( w80features, new SubProgressMonitor( monitor, 500 ) );
  }

  private void importW80Data( final FeatureList w80features, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( m_shapeDefaultSrs, w80features.size() + w80features.size() * m_handlers.size() );

    for( final Object object : w80features )
    {
      final Feature feature = (Feature) object;

      final IWProfPoint point = m_pointFactory.newPoint( feature );

      for( final IWProfContentHandler creator : m_handlers )
        creator.newPoint( point );

      ProgressUtilities.worked( monitor, 1 );
    }

    for( final IWProfContentHandler creator : m_handlers )
    {
      creator.finished();
      ProgressUtilities.worked( monitor, w80features.size() );
    }
  }

}
