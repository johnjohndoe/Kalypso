/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Date;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.Range;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Triangulates the given profile (segments) suing the indicated waterlevel as height. // * @return The min/max range of
 * the waterlevel. <code>null</code>, if no triangulation has been created.
 *
 * @author Monika Thül
 */
public class BreakLinesWriter implements IWspmConstants
{
  private Range<Double> m_range;

  private final TuhhReachProfileSegment[] m_segments;

  private final TupleResult m_result;

  private final String m_componentStation;

  private final String m_componentWaterlevel;

  private final Double m_epsThinning;

  private GMLWorkspace m_triangleWorkspace;

  private GMLWorkspace m_breaklinesWorkspace;

  public BreakLinesWriter( final TuhhReachProfileSegment[] segments, final TupleResult result, final String componentStation, final String componentWaterlevel, final Double epsThinning )
  {
    m_segments = segments;
    m_result = result;
    m_componentStation = componentStation;
    m_componentWaterlevel = componentWaterlevel;
    m_epsThinning = epsThinning;
  }

  private void init( ) throws GMLSchemaException, GM_Exception
  {
    if( m_triangleWorkspace != null || m_breaklinesWorkspace != null )
      return;

    final Map<Double, Double> wspMap = BreakLinesHelper.createWspMap( m_result, m_componentStation, m_componentWaterlevel );
    if( m_segments.length == 0 )
      return;

    m_triangleWorkspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPMCOMMONS, "TriangulatedSurfaceFeature" ), null, null ); //$NON-NLS-1$
    final String defaultCrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final GM_TriangulatedSurface surface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_TriangulatedSurface( defaultCrs );
    final Feature triangleFeature = m_triangleWorkspace.getRootFeature();
    triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "triangulatedSurfaceMember" ), surface ); //$NON-NLS-1$
    // TODO: set tin name
    NamedFeatureHelper.setDescription( triangleFeature, Messages.getString( "BreakLinesHelper.0" ) ); //$NON-NLS-1$
    NamedFeatureHelper.setName( triangleFeature, Messages.getString( "BreakLinesHelper.1" ) ); //$NON-NLS-1$
    triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "unit" ), "NN+m" ); //$NON-NLS-1$ //$NON-NLS-2$
    triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "parameter" ), "h" ); //$NON-NLS-1$ //$NON-NLS-2$
    triangleFeature.setProperty( new QName( NS_WSPMCOMMONS, "date" ), DateUtilities.toXMLGregorianCalendar( new Date() ) ); //$NON-NLS-1$

    m_breaklinesWorkspace = FeatureFactory.createGMLWorkspace( new QName( NS_WSPM_BREAKLINE, "BreaklineCollection" ), null, null ); //$NON-NLS-1$
    final Feature rootFeature = m_breaklinesWorkspace.getRootFeature();

    final String gmlVersion = rootFeature.getFeatureType().getGMLSchema().getGMLVersion();

    // debug
    GM_Curve lastProfile = null;
    Range<Double> range = null;
    for( final TuhhReachProfileSegment reach : m_segments )
    {
      final GM_Curve geometry = reach.getGeometry();
      final BigDecimal station = reach.getStation();
      if( geometry == null ) // ignore profiles without geometry
        continue;

      final Double wsp = wspMap.get( station.doubleValue() );

      final GM_Curve thinProfile = thinnedOutClone( geometry, m_epsThinning, gmlVersion );
      if( wsp != null )
      {
        if( range == null )
          range = Range.is( wsp );
        else
          range = Range.between( Math.min( wsp, range.getMinimum() ), Math.max( wsp, range.getMaximum() ) );

        // ignore profiles without result (no value in length section). This can occur if the
        // simulation does not cover the whole reach.
        final GM_Curve newProfile = GeometryUtilities.setValueZ( thinProfile.getAsLineString(), wsp );

        /* Triangulate two adjacent profiles */
        if( lastProfile != null )
        {
          final GM_Position[] polygonPosesClosed = GeometryUtilities.getPolygonfromCurves( lastProfile, newProfile );

          // Write the curve as breakline into breakline file
          final GM_Curve polygoneRing = GeometryFactory.createGM_Curve( polygonPosesClosed, defaultCrs );
          final Feature ringFeature = FeatureHelper.addFeature( rootFeature, new QName( NS_WSPM_BREAKLINE, "breaklineMember" ), new QName( NS_WSPM_BREAKLINE, "Breakline" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          ringFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "geometry" ), polygoneRing ); //$NON-NLS-1$
          ringFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "station" ), station ); //$NON-NLS-1$
          ringFeature.setProperty( new QName( NS_WSPM_BREAKLINE, "wsp" ), wsp ); //$NON-NLS-1$

          // Interpolate triangles between two adjacent curves and add them to the triangulated surface
          final GM_Position[] polygonPosesOpen = ArrayUtils.remove( polygonPosesClosed, polygonPosesClosed.length - 1 );
          final GM_Position[][] triangles = GeometryUtilities.triangulateRing( polygonPosesOpen );
          for( final GM_Position[] triangle : triangles )
          {
            final GM_Triangle gmTriangle = GeometryFactory.createGM_Triangle( triangle, defaultCrs );
            surface.add( gmTriangle );
          }
        }

        lastProfile = newProfile;
      }
    }

    m_range = range;
  }

  private static GM_Curve thinnedOutClone( final GM_Curve curve, final Double epsThinning, final String gmlVersion )
  {
    try
    {
      if( epsThinning > 0.0 )
      {
        // TODO thin out: sollten "Zwangspunkte" wie Marker fï¿½r Trennflï¿½chen / durchstrï¿½mte Bereiche erhalten
        // bleiben?
        return GeometryUtilities.getThinnedCurve( curve, epsThinning );
      }
      else
      {
        final IMarshallingTypeHandler lineStringTypeHandler = MarshallingTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForTypeName( GM_LineString.LINE_STRING_ELEMENT );
        final Object cloneObject = lineStringTypeHandler.cloneObject( curve, gmlVersion );
        return (GM_Curve) cloneObject;
      }
    }
    catch( final CloneNotSupportedException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return curve;
  }

  public void writeBreaklines( final File outputFile ) throws GMLSchemaException, GM_Exception, IOException, GmlSerializeException
  {
    init();

    GmlSerializer.serializeWorkspace( outputFile, m_breaklinesWorkspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
  }

  public void writeTin( final File outputFile ) throws IOException, GmlSerializeException, GMLSchemaException, GM_Exception
  {
    init();

    GmlSerializer.serializeWorkspace( outputFile, m_triangleWorkspace, IWspmTuhhConstants.WSPMTUHH_CODEPAGE );
  }

  public Range<Double> getRange( ) throws GMLSchemaException, GM_Exception
  {
    init();

    return m_range;
  }
}