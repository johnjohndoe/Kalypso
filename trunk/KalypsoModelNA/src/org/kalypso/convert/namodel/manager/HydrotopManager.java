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

package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author huebsch
 */
public class HydrotopManager extends AbstractManager
{
  final NAConfiguration m_conf;

  final Hashtable<String, Double> m_landuseSealingRateMap = new Hashtable<String, Double>();

  public HydrotopManager( final NAConfiguration conf ) throws IOException
  {
    super( conf.getHydrotopFormatURL() );
    m_conf = conf;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( int id, IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {

    return null;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace hydWorkspace, GMLWorkspace modelWorkspace, GMLWorkspace parameterWorkspace ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();
    // Catchment
    Feature modelRootFeature = modelWorkspace.getRootFeature();
    Feature modelCol = (Feature) modelRootFeature.getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    List catchmentList = (List) modelCol.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );

    Feature parameterRootFeature = parameterWorkspace.getRootFeature();
    List landuseList = (List) parameterRootFeature.getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    Iterator landuseIter = landuseList.iterator();
    while( landuseIter.hasNext() )
    {
      final Feature landuseFE = (Feature) landuseIter.next();

      final IRelationType rt = (IRelationType) landuseFE.getFeatureType().getProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK );
      final Feature linkedSealingFE = parameterWorkspace.resolveLink( landuseFE, rt );
      final Double sealingRate = (Double) linkedSealingFE.getProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING );
      final String landuseName = m_conf.getLanduseFeatureShortedName( landuseFE.getName() );
      if( m_landuseSealingRateMap.containsKey( landuseName ) )
        m_conf.getLogger().log( Level.WARNING, String.format( Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.0" ), landuseName ) ); //$NON-NLS-1$
      else
        m_landuseSealingRateMap.put( landuseName, sealingRate );
    }

    Iterator catchmentIter = catchmentList.iterator();
    // vollst‰ndige HydrotopList
    Feature rootFeature = hydWorkspace.getRootFeature();
    FeatureList hydList = (FeatureList) rootFeature.getProperty( NaModelConstants.HYDRO_MEMBER );

    final List soilTypeList = (List) parameterRootFeature.getProperty( NaModelConstants.PARA_SOILTYPE_MEMBER );
    asciiBuffer.getHydBuffer().append( Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.2" ) ).append( "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    while( catchmentIter.hasNext() )
    {
      final Feature catchmentFE = (Feature) catchmentIter.next();
      if( asciiBuffer.writeFeature( catchmentFE ) ) // do it only for relevant catchments
      {
        final List<String> hydIdList = new ArrayList<String>();
        final List<String> hydrotopOutputList = new ArrayList<String>();
        double totalSealedArea = 0.0;
        double totalUnsealedArea = 0.0;
        double totalArea = 0.0;
        final GM_Object tGGeomProp = (GM_Object) catchmentFE.getProperty( NaModelConstants.CATCHMENT_GEOM_PROP );
        final Geometry catchmentGeometry = JTSAdapter.export( tGGeomProp );

        // Hydrotope im TeilgebietsEnvelope
        final List<Hydrotop> hydInEnvList = hydList.query( catchmentFE.getBoundedBy(), null );
        int hydrotopAsciiID = 0;
        for( final Hydrotop hydrotop : hydInEnvList )
        {
          final Geometry hydrotopGeometry = JTSAdapter.export( hydrotop.getGeometry() );
          if( catchmentGeometry.contains( hydrotopGeometry.getInteriorPoint() ) )
          {
            final double hydrotopArea = hydrotopGeometry.getArea();
            final String landuseName = hydrotop.getLanduse();
            final Double landuseSealing = m_landuseSealingRateMap.get( m_conf.getLanduseFeatureShortedName( landuseName ) );
            if( landuseSealing == null )
            {
              final String msg = String.format( "Landuse class '%s' referenced by hydrotop '%s' is not defined. Calculation aborted.", hydrotop.getLanduse(), hydrotop.getId() );
              m_conf.getLogger().severe( msg );
              throw new SimulationException( msg );
            }
            final double combinedSealingPercentage = hydrotop.getCorrSealing() / 100.0 * landuseSealing.doubleValue();
            final double hydrotopUnsealedArea = hydrotopArea * (1.0 - combinedSealingPercentage);
            totalSealedArea += hydrotopArea * combinedSealingPercentage;
            totalUnsealedArea += hydrotopUnsealedArea;
            totalArea += hydrotopArea;
            String soilType = hydrotop.getSoilType();
            for( final Object object : soilTypeList )
            {
              final Feature f = (Feature) object;
              if( soilType.equals( f.getId() ) )
              {
                soilType = f.getName();
                break;
              }
            }
            hydrotopOutputList.add( String.format( Locale.US, "%10.2f%50s%50s%16g%16g%8d%10f%4d", hydrotopUnsealedArea, m_conf.getLanduseFeatureShortedName( hydrotop.getLanduse()), soilType, hydrotop.getMaxPerkolationRate(), hydrotop.getGWFactor(), ++hydrotopAsciiID, combinedSealingPercentage, hydrotop.getAsciiHydrotopType() ) ); //$NON-NLS-1$
            hydIdList.add( hydrotop.getId() );
          }
        }

        // TODO: throw exception (to the user), if writing of hydrotope file is checked (testing!!!)
        final double fehler = Math.abs( catchmentGeometry.getArea() - totalArea );
        final double fehlerinProzent = 100.0 * fehler / totalArea;
        if( fehlerinProzent > 1.0 )
          m_conf.getLogger().log( Level.WARNING, String.format( Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.3" ), totalArea, catchmentFE.getId(), catchmentGeometry.getArea(), fehler, fehlerinProzent ) );

        asciiBuffer.getHydBuffer().append( String.format( Locale.US, "%d %d %g %g %g\n", idManager.getAsciiID( catchmentFE ), hydrotopOutputList.size(), totalSealedArea, totalUnsealedArea, totalArea ) ); //$NON-NLS-1$
        for( final String line : hydrotopOutputList )
          asciiBuffer.getHydBuffer().append( line ).append( "\n" ); //$NON-NLS-1$

        idManager.addHydroInfo( catchmentFE, hydIdList );
      }
    }
  }
}
