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
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof;
import org.kalypso.convert.namodel.schema.binding.suds.ISwale;
import org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
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

  private class HydrotopSudsAsciiDescriptor
  {
    private class AreaDescriptor
    {
      private Double m_naturalAreaPercentage = 0.0;

      private Double m_sealedAreaPercentage = 0.0;

      private final boolean m_bothSet;

      public AreaDescriptor( final double naturalAreaPercentage, final double sealedAreaPercentage )
      {
        m_naturalAreaPercentage = naturalAreaPercentage;
        m_sealedAreaPercentage = sealedAreaPercentage;
        m_bothSet = true;
      }

      public AreaDescriptor( final double sealedAreaPercentage )
      {
        m_sealedAreaPercentage = sealedAreaPercentage;
        m_bothSet = false;
      }

      public String getAscii( )
      {
        if( m_bothSet )
          return String.format( "%.3f %.3f", m_naturalAreaPercentage, m_sealedAreaPercentage ); //$NON-NLS-1$
        return String.format( "%.3f", m_sealedAreaPercentage ); //$NON-NLS-1$
      }
    }

    private final Map<String, AreaDescriptor> m_map = new HashMap<String, AreaDescriptor>();

    public HydrotopSudsAsciiDescriptor( )
    {
      m_map.put( "10", new AreaDescriptor( 0.0 ) ); //$NON-NLS-1$
      m_map.put( "11", new AreaDescriptor( 0.0 ) ); //$NON-NLS-1$
      m_map.put( "12", new AreaDescriptor( 0.0 ) ); //$NON-NLS-1$
      m_map.put( "13", new AreaDescriptor( 0.0 ) ); //$NON-NLS-1$
      m_map.put( "20", new AreaDescriptor( 0.0, 0.0 ) ); //$NON-NLS-1$
      m_map.put( "21", new AreaDescriptor( 0.0, 0.0 ) ); //$NON-NLS-1$
      m_map.put( "30", new AreaDescriptor( 0.0, 0.0 ) ); //$NON-NLS-1$
      m_map.put( "31", new AreaDescriptor( 0.0, 0.0 ) ); //$NON-NLS-1$
      m_map.put( "40", new AreaDescriptor( 0.0 ) ); //$NON-NLS-1$
      m_map.put( "41", new AreaDescriptor( 0.0 ) ); //$NON-NLS-1$
    }

    public void addSuds( final String type, final double naturalAreaPercentage, final double sealedAreaPercentage )
    {
      m_map.put( type, new AreaDescriptor( naturalAreaPercentage, sealedAreaPercentage ) );
    };

    public void addSuds( final String type, final double sealedAreaPercentage )
    {
      m_map.put( type, new AreaDescriptor( sealedAreaPercentage ) );
    };

    public String getAscii( )
    {
      return String.format( "%s %s %s %s %s %s %s %s %s %s", m_map.get( "10" ).getAscii(), m_map.get( "11" ).getAscii(), m_map.get( "12" ).getAscii(), m_map.get( "13" ).getAscii(), m_map.get( "20" ).getAscii(), m_map.get( "21" ).getAscii(), m_map.get( "30" ).getAscii(), m_map.get( "31" ).getAscii(), m_map.get( "40" ).getAscii(), m_map.get( "41" ).getAscii() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$ //$NON-NLS-11$
    }

  }

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
        m_conf.getLogger().log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.0", landuseName ) ); //$NON-NLS-1$
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
        boolean anySuds = false;
        final List<String> hydIdList = new ArrayList<String>();
        final List<String> hydrotopOutputList = new ArrayList<String>();
        double totalHydrotopNaturalArea = 0.0;
        double totalHydrotopSealedArea = 0.0;
        double totalSudsNaturalArea = 0.0;
        double totalSudsSealedArea = 0.0;
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
              final String msg = ""; //$NON-NLS-1$
              m_conf.getLogger().severe( msg );
              throw new SimulationException( msg );
            }
            final double combinedSealingRate = hydrotop.getCorrSealing() * landuseSealing.doubleValue();
            final double hydrotopSealedArea = hydrotopArea * combinedSealingRate;
            final double hydrotopNaturalArea = hydrotopArea - hydrotopSealedArea;
            totalHydrotopSealedArea += hydrotopSealedArea;
            totalHydrotopNaturalArea += hydrotopNaturalArea;
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

            final IFeatureBindingCollection<Feature> sudsCollection = hydrotop.getSudCollection();
            if( sudsCollection.size() > 0 )
            {
              final HydrotopSudsAsciiDescriptor hydrotopSudsAsciiDescriptor = new HydrotopSudsAsciiDescriptor();
              for( final Feature sudsFeature : hydrotop.getSudCollection() )
              {
                if( sudsFeature instanceof ISwaleInfiltrationDitch )
                {
                  final ISwaleInfiltrationDitch suds = (ISwaleInfiltrationDitch) sudsFeature;
                  final double areaRate = suds.getAreaPercentage() / 100.0;
                  final double naturalAreaPart = hydrotopNaturalArea * areaRate;
                  final double sealedAreaPart = hydrotopSealedArea * areaRate;
                  hydrotopSudsAsciiDescriptor.addSuds( suds.getElementType(), naturalAreaPart, sealedAreaPart );
                  totalSudsNaturalArea += naturalAreaPart;
                  totalSudsSealedArea += sealedAreaPart;
                }
                else if( sudsFeature instanceof ISwale )
                {
                  final ISwale suds = (ISwale) sudsFeature;
                  final double areaRate = suds.getAreaPercentage() / 100.0;
                  final double naturalAreaPart = hydrotopNaturalArea * areaRate;
                  final double sealedAreaPart = hydrotopSealedArea * areaRate;
                  hydrotopSudsAsciiDescriptor.addSuds( suds.getElementType(), naturalAreaPart, sealedAreaPart );
                  totalSudsNaturalArea += naturalAreaPart;
                  totalSudsSealedArea += sealedAreaPart;
                }
                else if( sudsFeature instanceof IGreenRoof )
                {
                  final IGreenRoof suds = (IGreenRoof) sudsFeature;
                  final double areaRate = suds.getAreaPercentage() / 100.0;
                  final double sealedAreaPart = hydrotopSealedArea * areaRate;
                  hydrotopSudsAsciiDescriptor.addSuds( suds.getElementType(), sealedAreaPart );
                  totalSudsSealedArea += sealedAreaPart;
                }
              }
              hydrotopOutputList.add( String.format( Locale.US, "%.3f %s %s %.3g %.3g %d %.3f 1 %s", hydrotopNaturalArea, m_conf.getLanduseFeatureShortedName( hydrotop.getLanduse() ), soilType, hydrotop.getMaxPerkolationRate(), hydrotop.getGWFactor(), ++hydrotopAsciiID, combinedSealingRate, hydrotopSudsAsciiDescriptor.getAscii() ) ); //$NON-NLS-1$
              anySuds = true;
            }
            else
            {
              hydrotopOutputList.add( String.format( Locale.US, "%.3f %s %s %.3g %.3g %d %.3f 0", hydrotopNaturalArea, m_conf.getLanduseFeatureShortedName( hydrotop.getLanduse() ), soilType, hydrotop.getMaxPerkolationRate(), hydrotop.getGWFactor(), ++hydrotopAsciiID, combinedSealingRate ) ); //$NON-NLS-1$
            }

            hydIdList.add( hydrotop.getId() );
          }
        }

        // TODO: throw exception (to the user), if writing of hydrotope file is checked (testing!!!)
        final double fehler = Math.abs( catchmentGeometry.getArea() - totalHydrotopNaturalArea );
        final double fehlerinProzent = 100.0 * fehler / totalHydrotopNaturalArea;
        if( fehlerinProzent > 1.0 )
          m_conf.getLogger().log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.3", totalHydrotopNaturalArea, catchmentFE.getId(), catchmentGeometry.getArea(), fehler, fehlerinProzent ) ); //$NON-NLS-1$

        if( anySuds )
        {
          asciiBuffer.getHydBuffer().append( String.format( Locale.US, "%d %d %.3f %.3f %.3f 1 %.3f %.3f %.3f\n", idManager.getAsciiID( catchmentFE ), hydrotopOutputList.size(), totalHydrotopSealedArea, totalHydrotopNaturalArea, totalHydrotopSealedArea + totalHydrotopNaturalArea, totalSudsSealedArea, totalSudsNaturalArea, totalSudsSealedArea + totalSudsNaturalArea ) ); //$NON-NLS-1$
        }
        else
        {
          asciiBuffer.getHydBuffer().append( String.format( Locale.US, "%d %d %g %g %g\n", idManager.getAsciiID( catchmentFE ), hydrotopOutputList.size(), totalHydrotopSealedArea, totalHydrotopNaturalArea, totalHydrotopSealedArea + totalHydrotopNaturalArea ) ); //$NON-NLS-1$
        }
        for( final String line : hydrotopOutputList )
          asciiBuffer.getHydBuffer().append( line ).append( "\n" ); //$NON-NLS-1$

        idManager.addHydroInfo( catchmentFE, hydIdList );
      }
    }
  }
}
