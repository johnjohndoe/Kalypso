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
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;

/**
 * 
 * Handles the creation of a length section obs from several ({@link GM_TriangulatedSurface) data files and a river line (DAV format).
 * 
 * @author Thomas Jung
 * 
 */
public class LengthSectionHandler2d
{

  private final FeatureList m_riverFeatures;

  private final BigDecimal[] m_stationList;

  private final List<GM_TriangulatedSurface> m_surfaces;

  public LengthSectionHandler2d( final List<GM_TriangulatedSurface> surfaces, final FeatureList riverFeatures, final BigDecimal[] stationList )
  {
    m_surfaces = surfaces;
    m_riverFeatures = riverFeatures;
    m_stationList = stationList;

    generateLengthSection();
  }

  private void generateLengthSection( )
  {
    /* generate length section point list */

    // get "from" and "to" values for each segment
    for( Object object : m_riverFeatures )
    {
      Feature feature = (Feature) object;

      IPropertyType[] properties = feature.getFeatureType().getProperties();
      String localPart = properties[0].getQName().getLocalPart();

      final BigDecimal from;
      final BigDecimal to;
      Object propertyFrom = feature.getProperty( new QName( "namespace", "RIVER_A" ) );
      if( propertyFrom instanceof Long )
      {
        from = new BigDecimal( (Long) propertyFrom );
      }
      else
      {
        throw new ClassCastException( "Fehler bei Stationswert. Falscher Typ." );
      }

      Object propertyTo = feature.getProperty( new QName( "namespace", "RIVER_B" ) );
      if( propertyTo instanceof Long )
      {
        to = new BigDecimal( (Long) propertyFrom );
      }
      else
      {
        throw new ClassCastException( "Fehler bei Stationswert. Falscher Typ." );
      }

      GM_Object defaultGeometryProperty = feature.getDefaultGeometryProperty();
      GM_MultiCurve multiCurve = (GM_MultiCurve) defaultGeometryProperty;

      GM_Curve[] allCurves = multiCurve.getAllCurves();
      GM_Curve curve = allCurves[0];

      // jetzt hamma d Kurv :-)
      // Anhand Stationswerten Punkte auf Liniensegmenten abgreifen / erzeugen.
      for( int i = 0; i < m_stationList.length; i++ )
      {
        // check, if the station value lies between min max of the current curve
        if( m_stationList[i].compareTo( from ) == 1 && m_stationList[i].compareTo( to ) == -1 )
        {

        }

      }

    }
    // go through all line segments and collect the points corresponding to that list
    // get start and end station
    /* value collection for point list */

    // for each point get the values from the wsp and terrain surface
  }
}
