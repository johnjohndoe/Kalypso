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
package org.kalypso.kalypsomodel1d2d.internal.import2dm;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.bce.gis.io.zweidm.PolygonWithName;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Validates a {@link com.vividsolutions.jts.geom.Polygon} against an {@link IFEDiscretisationModel1d2d}, i.e. if thi
 * polygon can be safelyinserted into the model.
 * 
 * @author Gernot Belger
 */
public class PolygonDiscretisationValidator
{
  private final IFEDiscretisationModel1d2d m_model;

  public PolygonDiscretisationValidator( final IFEDiscretisationModel1d2d model )
  {
    m_model = model;
  }

  public String validate( final IPolygonWithName item )
  {
    final Envelope envelope = item.getEnvelope();
    final Polygon polygon = item.getPolygon();
    final int srid = polygon.getSRID();
    final GM_Envelope searchExtent = JTSAdapter.wrap( envelope, JTSAdapter.toSrs( srid ) );

    final List<IFE1D2DElement> query = m_model.queryElements( searchExtent, null );

    for( final IFE1D2DElement element : query )
    {
      final String message = validateElement( item, element );
      if( message != null )
        return message;

      // TODO: we should collect all problematic elements
    }

    return null;
  }

  private String validateElement( final IPolygonWithName item, final IFE1D2DElement element )
  {
    try
    {
      if( element instanceof IElement1D )
        return validateElement1D( item, (IElement1D)element );

      if( element instanceof IPolyElement )
        return validateElement2D( item, (IPolyElement)element );

      throw new IllegalStateException();
    }
    catch( final GM_Exception e )
    {
      return e.getLocalizedMessage();
    }
  }

  private String validateElement1D( final IPolygonWithName item, final IElement1D element ) throws GM_Exception
  {
    final IFE1D2DEdge edge = element.getEdge();
    final GM_Curve geometry = edge.getGeometry();
    final LineString line = (LineString)JTSAdapter.export( geometry );

    final Polygon polygon = item.getPolygon();

    if( polygon.intersects( line ) )
      return "touches an 1D-Element";

    return null;
  }

  private String validateElement2D( final IPolygonWithName item, final IPolyElement element ) throws GM_Exception
  {
    final GM_Polygon surface = element.getGeometry();
    final Polygon polygon = (Polygon)JTSAdapter.export( surface );

    final PolygonWithName elementItem = new PolygonWithName( element.getId(), polygon );

    final ModelTopologyValidator intersectionValidator = new ModelTopologyValidator();
    return intersectionValidator.validate( item, elementItem );
  }
}