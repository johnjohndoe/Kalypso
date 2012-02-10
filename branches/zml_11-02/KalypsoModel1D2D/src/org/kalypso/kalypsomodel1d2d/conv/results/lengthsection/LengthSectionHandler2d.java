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
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.model.wspm.schema.IWspmDictionaryConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.util.TupleResultIndex;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
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
 * 
 */
public class LengthSectionHandler2d
{
  public static void handle2DLenghtsection( IObservation<TupleResult> lsObs, GM_TriangulatedSurface surface, final LengthSectionParameters lengthSectionParameters, BigDecimal[] stationList, DOCUMENTTYPE documentType, boolean isKmValues, IProgressMonitor monitor )
  {
    final FeatureList riverFeatures = lengthSectionParameters.getRiverFeatures();
    final IPropertyType riverNamePropertyType = lengthSectionParameters.getRiverNamePropertyType();
    final String selectedRiverName = lengthSectionParameters.getSelectedRiverName();

    final IPropertyType fromStationPropertyType = lengthSectionParameters.getFromStationPropertyType();
    final IPropertyType toStationPropertyType = lengthSectionParameters.getToStationPropertyType();

    handle2DLenghtsection( lsObs, surface, riverFeatures, riverNamePropertyType, fromStationPropertyType, toStationPropertyType, selectedRiverName, stationList, documentType, isKmValues, monitor );
  }

  public static void handle2DLenghtsection( IObservation<TupleResult> lsObs, final GM_TriangulatedSurface surface, final FeatureList riverFeatures, final IPropertyType riverNamePropertyType, final IPropertyType fromStationPropertyType, final IPropertyType toStationPropertyType, final String riverName, final BigDecimal[] stationList, DOCUMENTTYPE documenttype, final boolean iskmValue, final IProgressMonitor monitor )
  {
    final Map<BigDecimal, GM_Point> pointList = getPointList( riverFeatures, stationList, riverNamePropertyType, fromStationPropertyType, toStationPropertyType, riverName, monitor );

    generateLengthSection( pointList, surface, lsObs, documenttype, iskmValue, monitor );
  }

  private static Map<BigDecimal, GM_Point> getPointList( FeatureList riverFeatures, final BigDecimal[] stationList, final IPropertyType riverNamePropertyType, final IPropertyType fromStationPropertyType, final IPropertyType toStationPropertyType, final String riverName, IProgressMonitor monitor )
  {
    // TODO: better monitoring

    Map<BigDecimal, GM_Point> pointList = new HashMap<BigDecimal, GM_Point>();

    for( Object object : riverFeatures )
    {
      Feature feature = (Feature) object;

      if( riverNamePropertyType != null )
      {
        final Object prop = feature.getProperty( riverNamePropertyType );

        if( !(prop instanceof String) )
          return pointList;

        final String featureRiverName = (String) prop;
        if( !featureRiverName.equals( riverName ) )
          continue;
      }

      // get "from" and "to" values for each segment
      final BigDecimal from;
      final BigDecimal to;

      final Object propertyFrom = feature.getProperty( fromStationPropertyType );

      if( propertyFrom instanceof Long )
        from = new BigDecimal( (Long) propertyFrom );
      else
        throw new ClassCastException( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d.0") ); //$NON-NLS-1$

      final Object propertyTo = feature.getProperty( toStationPropertyType );

      if( propertyTo instanceof Long )
        to = new BigDecimal( (Long) propertyTo );
      else
        throw new ClassCastException( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionHandler2d.1") ); //$NON-NLS-1$

      GM_Object defaultGeometryProperty = feature.getDefaultGeometryProperty();
      GM_MultiCurve multiCurve = (GM_MultiCurve) defaultGeometryProperty;

      if( multiCurve == null )
        continue;

      GM_Curve[] allCurves = multiCurve.getAllCurves();

      if( allCurves.length > 1 )
        continue;

      GM_Curve curve = allCurves[0];

      // jetzt hamma d Kurv :-)
      // Anhand Stationswerten Punkte auf Liniensegmenten abgreifen / erzeugen.
      for( int i = 0; i < stationList.length; i++ )
      {
        // calculate correction factor
        final BigDecimal stationLength = to.subtract( from );

        int stat2 = stationList[i].intValue();
        int toint = to.intValue();
        int fromint = from.intValue();

        // check, if the station value lies between min max of the current curve
        if( fromint <= stat2 && stat2 < toint )
        {
          BigDecimal stationLengthPosition = to.subtract( stationList[i] );
          double stat = stationLengthPosition.doubleValue() / stationLength.doubleValue() * 100;
          BigDecimal percentage = new BigDecimal( stat ).setScale( 4, BigDecimal.ROUND_HALF_UP );

          LineString linestring;
          try
          {
            linestring = (LineString) JTSAdapter.export( curve );
            Point point = JTSUtilities.pointOnLinePercent( linestring, percentage.intValue() );
            if( point == null )
              continue;
            GM_Point gmPoint = (GM_Point) JTSAdapter.wrap( point );
            if( gmPoint != null )
              pointList.put( stationList[i], gmPoint );
          }
          catch( GM_Exception e )
          {
            e.printStackTrace();
          }
        }
      }
    }
    return pointList;
  }

  private static void generateLengthSection( Map<BigDecimal, GM_Point> pointList, GM_TriangulatedSurface surface, IObservation<TupleResult> lsObs, DOCUMENTTYPE documenttype, final boolean iskmValue, IProgressMonitor monitor )
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

    TupleResultIndex m_tupleIndex = new TupleResultIndex( tuples, stationComp );

    // for each point get the values of the surfaces in the list of TrinagulatedSurface
    for( Map.Entry<BigDecimal, GM_Point> entry : pointList.entrySet() )
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
