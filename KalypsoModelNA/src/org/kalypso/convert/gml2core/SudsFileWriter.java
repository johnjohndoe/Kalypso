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
package org.kalypso.convert.gml2core;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.manager.BodentypManager;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.convert.namodel.schema.binding.suds.Greenroof;
import org.kalypso.convert.namodel.schema.binding.suds.Swale;
import org.kalypso.convert.namodel.schema.binding.suds.SwaleInfiltrationDitch;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanaskovic
 */
public class SudsFileWriter extends AbstractCoreFileWriter
{
  private final NAConfiguration m_config;

  public SudsFileWriter( final NAConfiguration config )
  {
    super( config.getSwaleAndTrenchFile() );
    m_config = config;
  }

  /**
   * @see org.kalypso.convert.gml2core.AbstractCoreFileWriter#createContent()
   */
  @Override
  protected void createContent( ) throws Exception
  {
    if( m_config.getModelWorkspace() != null && m_config.getHydrotopeWorkspace() != null && m_config.getSudsWorkspace() != null )
    {
      final TreeMap<String, TreeMap<String, List<String>>> sudsMap = new TreeMap<String, TreeMap<String, List<String>>>();
      final Feature catchmentCollection = (Feature) m_config.getModelWorkspace().getRootFeature().getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
      final FeatureList catchmentList = (FeatureList) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );

      m_config.getHydrotopeWorkspace().getRootFeature();
      final FeatureList hydrotopFeatureList = (FeatureList) m_config.getHydrotopeWorkspace().getRootFeature().getProperty( NaModelConstants.HYDRO_MEMBER );

      for( final Object o : hydrotopFeatureList )
      {
        final Hydrotop hydrotop = (Hydrotop) o;
        if( hydrotop.getSudCollection().size() > 0 )
        {
          final GM_Object hydrotopGeometryProperty = hydrotop.getDefaultGeometryPropertyValue();
          final Geometry hydrotopGeometry = JTSAdapter.export( hydrotopGeometryProperty );
          final GM_Object hydrotopInteriorPoint = JTSAdapter.wrap( hydrotopGeometry.getInteriorPoint() );
          final List<Feature> list = catchmentList.query( hydrotopGeometryProperty.getEnvelope(), null );
          for( final Feature catchment : list )
            if( catchment.getDefaultGeometryPropertyValue().contains( hydrotopInteriorPoint ) )
            {
// final Feature strang = (Feature) catchment.getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
// final String drainageNodeName = (String) strang.getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );

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
// final Object landuseClassLink = landuse.getLanduse();
//                  final String landuseClassName = (landuseClassLink instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) landuseClassLink).getFeature().getName() : "MRS_N"; //$NON-NLS-1$

                  Double maxPercRate = m_config.getSudsAverageMaxPercRate( suds.getId() );
                  if( Double.isNaN( maxPercRate ) )
                    maxPercRate = suds.getMaxPercRate();
                  value.add( String.format( Locale.US, "%s %s %.4g %.4g", suds.getIdealLanduseName(), BodentypManager.getSwaleSoiltypeName( suds ), maxPercRate, suds.getPercentToGroundwater() ) ); //$NON-NLS-1$
                  value.add( String.format( Locale.US, "%.1f %.1f %.1f %.4g %.4g 0", (double) suds.getPipeDiameter(), (double) suds.getPipeKfValue(), suds.getPipeSlope() / 1000.0, suds.getPipeRoughness(), suds.getWidth() ) ); //$NON-NLS-1$
                }
                else if( f instanceof Swale )
                {
                  /**
                   * Not implemented yet in NA Core
                   */
                  final Swale suds = (Swale) f;
                  key = suds.getElementType();
// final Object landuseClassLink = landuse.getLanduse();
//                  final String landuseClassName = (landuseClassLink instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) landuseClassLink).getFeature().getName() : "Mulde_N"; //$NON-NLS-1$

                  value.add( String.format( Locale.US, "%s %s 2.5E-8 1.0", suds.getIdealLanduseName(), BodentypManager.getSwaleSoiltypeName( suds ) ) ); //$NON-NLS-1$
                  value.add( String.format( Locale.US, "%.3f 0", suds.getWidth() ) ); //$NON-NLS-1$
                }
                else if( f instanceof Greenroof )
                {
                  final Greenroof suds = (Greenroof) f;
                  key = suds.getElementType();
// final Object landuseClassLink = landuse.getLanduse();
//                  final String landuseClassName = (landuseClassLink instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) landuseClassLink).getFeature().getName() : "GRext_N"; //$NON-NLS-1$

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
      m_contentBuffer.setLength( 0 );
      for( final String catchment : sudsMap.keySet() )
      {
        m_contentBuffer.append( String.format( Locale.US, "# Catchment_NR %s\n", catchment ) ); //$NON-NLS-1$
        final TreeMap<String, List<String>> map = sudsMap.get( catchment );
        for( final String sudsID : map.keySet() )
        {
          final List<String> list = map.get( sudsID );
          if( list == null )
            continue;
          m_contentBuffer.append( String.format( Locale.US, "%s %s\n", catchment, sudsID ) ); //$NON-NLS-1$
          for( final String line : list )
            m_contentBuffer.append( line ).append( "\n" ); //$NON-NLS-1$
        }
        m_contentBuffer.append( String.format( Locale.US, "# Ende TG %s\n", catchment ) ); //$NON-NLS-1$
      }
    }
  }
}
