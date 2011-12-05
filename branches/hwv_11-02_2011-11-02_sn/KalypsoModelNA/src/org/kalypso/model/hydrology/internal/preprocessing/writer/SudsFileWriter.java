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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.suds.Greenroof;
import org.kalypso.model.hydrology.binding.suds.Swale;
import org.kalypso.model.hydrology.binding.suds.SwaleInfiltrationDitch;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanaskovic
 */
public class SudsFileWriter extends AbstractCoreFileWriter
{
  private final NaModell m_naModel;

  private final NAHydrotop m_hydrotopeCollection;

  private final GMLWorkspace m_sudsWorkspace;

  public SudsFileWriter( final NaModell naModel, final NAHydrotop hydrotopeCollection, final GMLWorkspace sudsWorkspace, final Logger logger )
  {
    super( logger );

    m_naModel = naModel;
    m_hydrotopeCollection = hydrotopeCollection;
    m_sudsWorkspace = sudsWorkspace;
  }

  /**
   * @see org.kalypso.convert.gml2core.AbstractCoreFileWriter#write(java.io.PrintWriter)
   */
  @Override
  protected void writeContent( final PrintWriter printWriter ) throws Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    if( m_naModel != null && m_hydrotopeCollection != null && m_sudsWorkspace != null )
    {
      final SortedMap<String, TreeMap<String, List<String>>> sudsMap = new TreeMap<String, TreeMap<String, List<String>>>();
      final IFeatureBindingCollection<Catchment> catchments = m_naModel.getCatchments();

      final MaxPercolationCalculator maxPercolationCalculator = new MaxPercolationCalculator( m_hydrotopeCollection );

      final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopeCollection.getHydrotopes();

      // FIXME: separate hashing logic from writing logic
      // FIXME: add the hashing logic into the HydroHash
      for( final IHydrotope hydrotop : hydrotopes )
      {
        if( hydrotop.getSudCollection().size() > 0 )
        {
          final GM_Object hydrotopGeometryProperty = hydrotop.getDefaultGeometryPropertyValue();
          final Geometry hydrotopGeometry = JTSAdapter.export( hydrotopGeometryProperty );
          final GM_Object hydrotopInteriorPoint = JTSAdapter.wrap( hydrotopGeometry.getInteriorPoint(), coordinateSystem );
          final List<Catchment> list = catchments.query( hydrotopGeometryProperty.getEnvelope() );
          for( final Catchment catchment : list )
          {
            if( catchment.getDefaultGeometryPropertyValue().contains( hydrotopInteriorPoint ) )
            {
              final String catchmentName = catchment.getName();
              if( !sudsMap.containsKey( catchmentName ) )
                sudsMap.put( catchmentName, new TreeMap<String, List<String>>() );
              final Feature[] sudsArray = hydrotop.getSuds();
              for( final Feature s : sudsArray )
              {
                final Feature f = s instanceof XLinkedFeature_Impl ? ((XLinkedFeature_Impl) s).getFeature() : s;
                final String key;
                final List<String> value = new ArrayList<String>();
                if( f instanceof SwaleInfiltrationDitch )
                {
                  /**
                   * # Mulden-Rigolen Data for the subcatchment 4500 # Format: # Catchment_NR. MR-Element_Type # Area of
                   * MR-Element [m≤] Landusetyp Soilprofil max.Perkolation [mm/d] Aufteilungsfaktor-Grundwasser[%] #
                   * diameter-Drainpipe[mm] kf-Drainpipe [mm/d] Slope-Drainpipe [prommille] Roughness Drainpipe [mm]
                   * width of the MR-Element[m] Nodenumber for the Draindischarge 4500 30 580. MRS_N mrs 2.8E-8 1.0 200.
                   * 4270. 0.003 2. 1.8 0 # ende MR TG 4500
                   */
                  final SwaleInfiltrationDitch suds = (SwaleInfiltrationDitch) f;
                  key = suds.getElementType();

                  final double maxPercRate = maxPercolationCalculator.getSudsAverageMaxPercRate( suds );
                  value.add( String.format( Locale.US, "%s %s %.4g %.4g", suds.getIdealLanduseName(), BodentypWriter.getSwaleSoiltypeName( suds ), maxPercRate, suds.getPercentToGroundwater() ) ); //$NON-NLS-1$
                  value.add( String.format( Locale.US, "%.1f %.1f %.1f %.4g %.4g 0", (double) suds.getPipeDiameter(), (double) suds.getPipeKfValue(), suds.getPipeSlope() / 1000.0, suds.getPipeRoughness(), suds.getWidth() ) ); //$NON-NLS-1$
                }
                else if( f instanceof Swale )
                {
                  /**
                   * Not implemented yet in NA Core
                   */
                  final Swale suds = (Swale) f;
                  key = suds.getElementType();

                  value.add( String.format( Locale.US, "%s %s 2.5E-8 1.0", suds.getIdealLanduseName(), BodentypWriter.getSwaleSoiltypeName( suds ) ) ); //$NON-NLS-1$
                  value.add( String.format( Locale.US, "%.3f 0", suds.getWidth() ) ); //$NON-NLS-1$
                }
                else if( f instanceof Greenroof )
                {
                  final Greenroof suds = (Greenroof) f;
                  key = suds.getElementType();

                  value.add( String.format( Locale.US, "%s %s 2.8E-10 1.0", suds.getIdealLanduseName(), suds.getUsageType().getSoilTypeID() ) ); //$NON-NLS-1$

                  // second line params:
                  // 1. Drainage pipe diameter [mm]
                  // 2. Overflow pipe diameter [mm]
                  // 3. Drainage pipe sand roughness [mm] - fixed to 2.0
                  // 4. Overflow pipe sand roughness [mm] - fixed to 2.0
                  // 5. Drainage area per pipe [m2] - fixed to 100.0
                  // 6. Overflow height of the roof [mm] - fixed to 100.0, max value equals to layer thickness
                  // 7. Drainage node ID; 0 = default drainage node of the catchment
                  value.add( String.format( Locale.US, "%.1f %.1f 2.0 2.0 100.0 100.0 0", new Double( suds.getRainwaterPipeDiameter().toString() ), new Double( suds.getEmergencySpillPipeDiameter().toString() ) ) ); //$NON-NLS-1$
                }
                else
                  continue;
                // only one instance of certain suds type is allowed per catchment
                final Map<String, List<String>> map = sudsMap.get( catchmentName );
                map.put( key, value );
              }
            }
          }
        }
      }

      for( final String catchment : sudsMap.keySet() )
      {
        printWriter.format( Locale.US, "# Catchment_NR %s\n", catchment ); //$NON-NLS-1$
        final SortedMap<String, List<String>> map = sudsMap.get( catchment );
        for( final String sudsID : map.keySet() )
        {
          final List<String> list = map.get( sudsID );
          if( list == null )
            continue;
          printWriter.format( Locale.US, "%s %s\n", catchment, sudsID ); //$NON-NLS-1$
          for( final String line : list )
            printWriter.append( line ).append( "\n" ); //$NON-NLS-1$
        }
        printWriter.format( Locale.US, "# Ende TG %s\n", catchment ); //$NON-NLS-1$
      }
    }
  }
}
