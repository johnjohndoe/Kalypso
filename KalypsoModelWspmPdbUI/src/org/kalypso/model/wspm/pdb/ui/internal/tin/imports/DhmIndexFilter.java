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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Holger Albert
 */
public class DhmIndexFilter extends ViewerFilter
{
  public static final String PROPERTY_FILTER_TEXT = "filterText"; //$NON-NLS-1$

  public static final String PROPERTY_FILTER_QUERY = "filterQuery"; //$NON-NLS-1$

  private final Viewer m_viewer;

  private final String m_dbCoordinateSystem;

  private String m_filterText;

  private boolean m_filterQuery;

  public DhmIndexFilter( final Viewer viewer, final String dbCoordinateSystem, final String filterText, final boolean filterQuery )
  {
    m_viewer = viewer;
    m_dbCoordinateSystem = dbCoordinateSystem;

    setFilterText( filterText );
    setFilterQuery( filterQuery );
  }

  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( !(element instanceof DhmIndex) )
      return false;

    final DhmIndex dhmIndex = (DhmIndex) element;

    final boolean text = filterByText( dhmIndex );
    final boolean query = filterByQuery( dhmIndex );

    return text && query;
  }

  private boolean filterByText( final DhmIndex dhmIndex )
  {
    if( StringUtils.isEmpty( m_filterText ) )
      return true;

    if( filterByText( dhmIndex.getName() ) )
      return true;

    if( filterByText( dhmIndex.getFilename() ) )
      return true;

    if( filterByText( dhmIndex.getMimeType() ) )
      return true;

    if( filterByText( dhmIndex.getEditingUser() ) )
      return true;

    if( filterByText( dhmIndex.getSource() ) )
      return true;

    if( filterByText( dhmIndex.getEditor() ) )
      return true;

    if( filterByText( dhmIndex.getMeasurementAccuracy() ) )
      return true;

    if( filterByText( dhmIndex.getDescription() ) )
      return true;

    if( filterByText( dhmIndex.getCopyright() ) )
      return true;

    if( filterByText( dhmIndex.getSrid() ) )
      return true;

    return false;
  }

  private boolean filterByText( final String text )
  {
    if( StringUtils.isEmpty( text ) )
      return false;

    return text.toLowerCase().contains( m_filterText );
  }

  private boolean filterByQuery( final DhmIndex dhmIndex )
  {
    try
    {
      if( !m_filterQuery )
        return true;

      final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final IMapPanel mapPanel = MapHandlerUtils.getMapPanel( currentState );
      if( mapPanel == null )
        return false;

      final Polygon location = dhmIndex.getLocation();
      if( location == null )
        return false;

      final IGeoTransformer geoTransformer = GeoTransformerFactory.getGeoTransformer( m_dbCoordinateSystem );

      final GM_Envelope mapBoundingBox = mapPanel.getBoundingBox();
      final GM_Envelope transformedBoundingBox = geoTransformer.transform( mapBoundingBox );

      final Envelope boundingBox = JTSAdapter.export( transformedBoundingBox );

      final GeometryFactory factory = new GeometryFactory( location.getPrecisionModel(), location.getSRID() );
      final Geometry boundingBoxGemoetry = factory.toGeometry( boundingBox );

      return location.intersects( boundingBoxGemoetry );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
      return false;
    }
  }

  public String getFilterText( )
  {
    return m_filterText;
  }

  public boolean isFilterQuery( )
  {
    return m_filterQuery;
  }

  public void setFilterText( final String filterText )
  {
    m_filterText = StringUtils.isBlank( filterText ) ? null : filterText.toLowerCase();

    if( m_viewer != null )
      m_viewer.refresh();
  }

  public void setFilterQuery( final boolean filterQuery )
  {
    m_filterQuery = filterQuery;

    if( m_viewer != null )
      m_viewer.refresh();
  }
}