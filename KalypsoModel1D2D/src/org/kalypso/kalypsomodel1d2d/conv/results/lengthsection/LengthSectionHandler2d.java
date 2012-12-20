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
package org.kalypso.kalypsomodel1d2d.conv.results.lengthsection;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 *
 * Handles the creation of a length section obs from several ({@link GM_TriangulatedSurface) data files and a river line (DAV format).
 *
 * @author Thomas Jung
 */
public class LengthSectionHandler2d
{
  public static void handle2DLenghtsection( final IObservation<TupleResult> lsObs, final GM_TriangulatedSurface surface, final LengthSectionHandlerParameters data, final BigDecimal[] stationList, final DOCUMENTTYPE documentType, final IProgressMonitor monitor )
  {
    final Map<BigDecimal, GM_Point> pointList = getPointList( data, stationList );

    final boolean useKmValues = data.getUseKmValues();
    generateLengthSection( pointList, surface, lsObs, documentType, useKmValues, monitor );
  }

  private static Map<BigDecimal, GM_Point> getPointList( final LengthSectionHandlerParameters data, final BigDecimal[] stationList )
  {
    // TODO: better monitoring

    final Map<BigDecimal, GM_Point> pointList = new HashMap<>();

    final Feature[] riverFeatures = data.getSelectedRivers();
    final IValuePropertyType fromStationPropertyType = data.getStationFromProperty();
    final IValuePropertyType toStationPropertyType = data.getStationToProperty();


    for( final Feature feature : riverFeatures )
    {
      final GM_Curve curve = data.getGeometry( feature );
      if( curve == null )
        continue;

      // get "from" and "to" values for each segment
      final BigDecimal from = data.getNumberProperty( feature, fromStationPropertyType, 0.0 );
      final BigDecimal to = data.getNumberProperty( feature, toStationPropertyType, curve.getLength() );

      // Anhand Stationswerten Punkte auf Liniensegmenten abgreifen / erzeugen.

      final BigDecimal definedCurveLength = to.subtract( from );

      final double toDouble = to.doubleValue();
      final double fromDouble = from.doubleValue();

      for( final BigDecimal currentStation : stationList )
      {
        // calculate correction factor
        final double currentStationDouble = currentStation.doubleValue();

        // check, if the station value lies between min max of the current curve
        if( fromDouble <= currentStationDouble && currentStationDouble <= toDouble )
        {
          final BigDecimal lengthFromBeginning = currentStation.subtract( from );
          final double station = lengthFromBeginning.doubleValue() / definedCurveLength.doubleValue() * 100;
          final BigDecimal percentage = new BigDecimal( station ).setScale( 4, BigDecimal.ROUND_HALF_UP );

          LineString linestring;
          try
          {
            linestring = (LineString) JTSAdapter.export( curve );
            final Point point = JTSUtilities.pointOnLinePercent( linestring, percentage.doubleValue() );
            if( point == null )
              continue;
            final GM_Point gmPoint = (GM_Point) JTSAdapter.wrap( point );
            if( gmPoint != null )
              pointList.put( currentStation, gmPoint );
          }
          catch( final GM_Exception e )
          {
            e.printStackTrace();
          }
        }
      }
    }

    return pointList;
  }

  private static void generateLengthSection( final Map<BigDecimal, GM_Point> pointList, final GM_TriangulatedSurface surface, final IObservation<TupleResult> lsObs, final DOCUMENTTYPE documenttype, final boolean iskmValue, final IProgressMonitor monitor )
  {
    /* generate length section point list */

    // go through all line segments and collect the points corresponding to that list
    /* value collection for point list */

    final TupleResult tuples = lsObs.getResult();

    final IComponent[] components = tuples.getComponents();

    final IComponent stationComp = ComponentUtilities.findComponentByID( components, IWspmDictionaryConstants.LS_COMPONENT_STATION );

    final IComponent thalComp = ComponentUtilities.findComponentByID( components, IWspmDictionaryConstants.LS_COMPONENT_GROUND );
    final IComponent waterlevelComp = ComponentUtilities.findComponentByID( components, IWspmDictionaryConstants.LS_COMPONENT_WATERLEVEL );
    final IComponent velocityComp = ComponentUtilities.findComponentByID( components, IWspmDictionaryConstants.LS_COMPONENT_VELOCITY );
    final IComponent dischargeComp = ComponentUtilities.findComponentByID( components, IWspmDictionaryConstants.LS_COMPONENT_RUNOFF );

    final int stationIndex = tuples.indexOfComponent( stationComp );
    final int thalwegIndex = tuples.indexOfComponent( thalComp );
    final int waterlevelIndex = tuples.indexOfComponent( waterlevelComp );
    final int velocityIndex = tuples.indexOfComponent( velocityComp );
    final int dischargeIndex = tuples.indexOfComponent( dischargeComp );

    final TupleResultIndex m_tupleIndex = new TupleResultIndex( tuples, stationComp );

    // for each point get the values of the surfaces in the list of TrinagulatedSurface

    for( final Map.Entry<BigDecimal, GM_Point> entry : pointList.entrySet() )
    {
      BigDecimal station = entry.getKey();

      // station values are in meter and have to be divided by 1000 in order to fit the chart view axis, which unit
      // is km
      if( !iskmValue )
        station = station.divide( new BigDecimal( "1000.0" ), 4, BigDecimal.ROUND_HALF_UP ); //$NON-NLS-1$

      // get the value from the TriangulatedSurface
      final Double v = surface.getValue( entry.getValue() );
      if( v.isNaN() )
        continue;

      final BigDecimal value = new BigDecimal( v ).setScale( 4, BigDecimal.ROUND_HALF_UP );

      // look for the record
      IRecord record = m_tupleIndex.getRecord( station );

      // if result is null, create a new one
      if( record == null )
      {
        record = tuples.createRecord();
        record.setValue( stationIndex, station );

        // REMARK: we have to add a dummy discharge values of 0.0 because the ChartView can not handle null
        // entries in obs.
        record.setValue( dischargeIndex, new BigDecimal( "0.00" ) ); //$NON-NLS-1$

        switch( documenttype )
        {
          case tinTerrain:
            record.setValue( thalwegIndex, value );
            break;

          case tinWsp:
            record.setValue( waterlevelIndex, value );
            break;

          case tinVelo:
            record.setValue( velocityIndex, value );
            break;

          default:
            break;
        }

        tuples.add( record );
      }
      else
      {
        switch( documenttype )
        {
          case tinTerrain:
            record.setValue( thalwegIndex, value );
            break;

          case tinWsp:
            record.setValue( waterlevelIndex, value );
            break;

          case tinVelo:
            record.setValue( velocityIndex, value );
            break;

          default:
            break;
        }
      }
    }
  }

}
