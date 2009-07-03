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
import java.util.Map;
import java.util.TreeMap;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.schema.binding.Landuse;
import org.kalypso.convert.namodel.schema.binding.LanduseCollection;
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
    if( m_config.getModelWorkspace() != null && m_config.getLanduseWorkspace() != null && m_config.getSudsWorkspace() != null )
    {
      final TreeMap<String, TreeMap<String, List<String>>> sudsMap = new TreeMap<String, TreeMap<String, List<String>>>();
      final Feature catchmentCollection = (Feature) m_config.getModelWorkspace().getRootFeature().getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
      final FeatureList catchmentList = (FeatureList) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );

      final LanduseCollection landuseCollection = (LanduseCollection) m_config.getLanduseWorkspace().getRootFeature();
      for( final Landuse landuse : landuseCollection.getLanduses() )
      {
        if( landuse.getSudCollection().size() > 0 )
        {
          final GM_Object landuseGeometryProperty = landuse.getGeometry();
          final Geometry landuseGeometry = JTSAdapter.export( landuseGeometryProperty );
          final GM_Object landuseInteriorPoint = JTSAdapter.wrap( landuseGeometry.getInteriorPoint() );
          final List<Feature> list = catchmentList.query( landuseGeometryProperty.getEnvelope(), null );
          for( final Feature catchment : list )
            if( catchment.getDefaultGeometryPropertyValue().contains( landuseInteriorPoint ) )
            {
              final String catchmentName = catchment.getName();
              if( !sudsMap.containsKey( catchmentName ) )
                sudsMap.put( catchmentName, new TreeMap<String, List<String>>() );
              final Feature[] suds = landuse.getSuds();
              for( final Feature s : suds )
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
                  final SwaleInfiltrationDitch sud = (SwaleInfiltrationDitch) f;
                  key = sud.getElementType().toString();
                  final double area = landuseGeometry.getArea() * sud.getAreaPercentage();
                  final Object landuseClassLink = landuse.getLanduse();
                  final String landuseClassName = (landuseClassLink instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) landuseClassLink).getFeature().getName() : "MRS_N";
                  sud.getLanduseFileName();

                  value.add( String.format( "%.4g %s mrs %.4g %.4g", area, landuseClassName, sud.getMaxPercRate(), sud.getPercentToGroundwater() ) );
                  value.add( String.format( "%d %d %d %.4g %.4g 0", sud.getPipeDiameter(), sud.getPipeKfValue(), sud.getPipeSlope(), sud.getPipeRoughness(), sud.getWidth() ) );
                }
                else if( f instanceof Swale )
                {
                  /**
                   * Not implemented yet in NA Core
                   */
                  final Swale sud = (Swale) f;
                  key = sud.getElementType().toString();
                }
                else if( f instanceof Greenroof )
                {
                  /**
                   * Not implemented yet in NA Core
                   */
                  final Greenroof sud = (Greenroof) f;
                  key = sud.getElementType().toString();
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
        m_contentBuffer.append( String.format( "# Catchment_NR %s\n", catchment ) );
        final TreeMap<String, List<String>> map = sudsMap.get( catchment );
        for( final String sudsID : map.keySet() )
        {
          final List<String> list = map.get( sudsID );
          if( list == null )
            continue;
          m_contentBuffer.append( String.format( "%s %s\n", catchment, sudsID ) );
          for( final String line : list )
            m_contentBuffer.append( line ).append( "\n" );
        }
        m_contentBuffer.append( String.format( "# Ende TG %s\n", catchment ) );
      }
    }
  }
}
