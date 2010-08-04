/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.logging.Logger;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof;
import org.kalypso.convert.namodel.schema.binding.suds.ISealing;
import org.kalypso.convert.namodel.schema.binding.suds.ISwale;
import org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * @author Dejan Antanaskovic
 */
public class HydrotopManager extends AbstractManager
{
  private final NAConfiguration m_conf;

  private final Map<String, Double> m_landuseSealingRateMap = new Hashtable<String, Double>();

  private final Logger m_logger;

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
          return String.format( Locale.US, "%.3f %.3f", m_naturalAreaPercentage, m_sealedAreaPercentage ); //$NON-NLS-1$
        return String.format( Locale.US, "%.3f", m_sealedAreaPercentage ); //$NON-NLS-1$
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
    }

    public void addSuds( final String type, final double sealedAreaPercentage )
    {
      m_map.put( type, new AreaDescriptor( sealedAreaPercentage ) );
    }

    public String getAscii( )
    {
      return String.format( Locale.US, "%s %s %s %s %s %s %s %s %s %s", m_map.get( "10" ).getAscii(), m_map.get( "11" ).getAscii(), m_map.get( "12" ).getAscii(), m_map.get( "13" ).getAscii(), m_map.get( "20" ).getAscii(), m_map.get( "21" ).getAscii(), m_map.get( "30" ).getAscii(), m_map.get( "31" ).getAscii(), m_map.get( "40" ).getAscii(), m_map.get( "41" ).getAscii() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$ //$NON-NLS-11$
    }
  }

  public HydrotopManager( final NAConfiguration conf, final Logger logger ) throws IOException
  {
    super( conf.getHydrotopFormatURL() );
    m_conf = conf;
    m_logger = logger;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( final int id, final IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( final URL url ) throws Exception
  {
    return null;
  }

  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace hydWorkspace, final GMLWorkspace modelWorkspace, final GMLWorkspace parameterWorkspace ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();
    // Catchment
    final Feature catchmentCollection = (Feature) modelWorkspace.getRootFeature().getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    final List<Feature> catchmentList = (List<Feature>) catchmentCollection.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );

    final List<Feature> landuseList = (List<Feature>) parameterWorkspace.getRootFeature().getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    final Iterator<Feature> landuseIter = landuseList.iterator();
    while( landuseIter.hasNext() )
    {
      final Feature landuseFE = landuseIter.next();
      final IRelationType rt = (IRelationType) landuseFE.getFeatureType().getProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK );
      final Feature linkedSealingFE = parameterWorkspace.resolveLink( landuseFE, rt );
      final Double sealingRate = (Double) linkedSealingFE.getProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING );
      final String landuseName = m_conf.getLanduseFeatureShortedName( landuseFE.getName() );
      if( m_landuseSealingRateMap.containsKey( landuseName ) )
        m_logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.0", landuseName ) ); //$NON-NLS-1$
      else
        m_landuseSealingRateMap.put( landuseName, sealingRate );
    }

    final Iterator<Feature> catchmentIter = catchmentList.iterator();
    // vollst�ndige HydrotopList
    final FeatureList hydList = (FeatureList) hydWorkspace.getRootFeature().getProperty( NaModelConstants.HYDRO_MEMBER );

    final List<Feature> soilTypeList = (List<Feature>) parameterWorkspace.getRootFeature().getProperty( NaModelConstants.PARA_SOILTYPE_MEMBER );
    asciiBuffer.getHydBuffer().append( Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.2" ) ).append( "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    while( catchmentIter.hasNext() )
    {
      final Feature catchmentFE = catchmentIter.next();
      if( asciiBuffer.isFeatureMakredForWrite( catchmentFE ) ) // do it only for relevant catchments
      {
        final int catchmentAsciiID = idManager.getAsciiID( catchmentFE );
        boolean anySuds = false;
        final List<String> hydIdList = new ArrayList<String>();
        final List<String> hydrotopOutputList = new ArrayList<String>();
        double totalHydrotopArea = 0.0;
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
          // FIXME: slow: is it really necessary to intersect hydrotopes with catchments? Doesnt the hydrotopes already
          // have the i of catchment? At least we should cache pointintrin / interior point to spped up
          final Geometry hydrotopGeometry = JTSAdapter.export( hydrotop.getGeometry() );
          if( catchmentGeometry.contains( hydrotopGeometry.getInteriorPoint() ) )
          {
            final double hydrotopArea = hydrotopGeometry.getArea();
            final String landuseName = hydrotop.getLanduse();
            final Double landuseSealing = m_landuseSealingRateMap.get( m_conf.getLanduseFeatureShortedName( landuseName ) );
            if( landuseSealing == null )
            {
              final String msg = "Unknown landuse found. Please re=create hydrotop file!"; //$NON-NLS-1$
              m_logger.severe( msg );
              throw new SimulationException( msg );
            }
            double totalSealingPercentage = landuseSealing.doubleValue() * hydrotop.getCorrSealing();
            double hydrotopSealedArea = hydrotopArea * totalSealingPercentage;
            double hydrotopNaturalArea = hydrotopArea - hydrotopSealedArea;
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

            final HydrotopSudsAsciiDescriptor hydrotopSudsAsciiDescriptor = new HydrotopSudsAsciiDescriptor();
            final IFeatureBindingCollection<Feature> sudsCollection = hydrotop.getSudCollection();
            if( sudsCollection.size() > 0 )
            {
              // first, fix the sealing
              for( final Feature sudsFeature : hydrotop.getSudCollection() )
              {
                final Feature feature = (sudsFeature instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) sudsFeature).getFeature() : sudsFeature;
                if( feature instanceof ISealing )
                {
                  final ISealing suds = (ISealing) feature;
                  totalSealingPercentage = totalSealingPercentage * suds.getSealingFactor();
                  hydrotopSealedArea = hydrotopArea * totalSealingPercentage;
                  hydrotopNaturalArea = hydrotopArea - hydrotopSealedArea;
                }
              }
              final double hydrotopNaturalAreaAfterUnsealing = hydrotopNaturalArea;
              final double hydrotopSealedAreaAfterUnsealing = hydrotopSealedArea;
              // than, apply other measures
              for( final Feature sudsFeature : hydrotop.getSudCollection() )
              {
                final Feature feature = (sudsFeature instanceof XLinkedFeature_Impl) ? ((XLinkedFeature_Impl) sudsFeature).getFeature() : sudsFeature;
                if( feature instanceof ISwaleInfiltrationDitch )
                {
                  final ISwaleInfiltrationDitch suds = (ISwaleInfiltrationDitch) feature;
                  final double naturalAreaRate = suds.getNaturalAreaPercentage() / 100.0;
                  final double drainingRateOfSealedArea = suds.getDrainedPercentageOfSealedArea() / 100.0;
                  final double sudsNaturalAreaPart = hydrotopNaturalAreaAfterUnsealing * naturalAreaRate;
                  final double sudsDrainedSealedAreaPart = hydrotopSealedAreaAfterUnsealing * drainingRateOfSealedArea;
                  hydrotopSudsAsciiDescriptor.addSuds( suds.getElementType(), sudsNaturalAreaPart, sudsDrainedSealedAreaPart );
                  hydrotopNaturalArea -= sudsNaturalAreaPart;
                  hydrotopSealedArea -= sudsDrainedSealedAreaPart;
                  totalSudsNaturalArea += sudsNaturalAreaPart;
                  totalSudsSealedArea += sudsDrainedSealedAreaPart;
                  m_conf.addSudsMaxPercRateMember( feature.getId(), hydrotop.getMaxPerkolationRate() );
                }
                else if( feature instanceof ISwale )
                {
                  final ISwale suds = (ISwale) feature;
                  final double naturalAreaRate = suds.getNaturalAreaPercentage() / 100.0;
                  final double drainingRateOfSealedArea = suds.getDrainedPercentageOfSealedArea() / 100.0;
                  final double sudsNaturalAreaPart = hydrotopNaturalAreaAfterUnsealing * naturalAreaRate;
                  final double sudsDrainedSealedAreaPart = hydrotopSealedAreaAfterUnsealing * drainingRateOfSealedArea;
                  hydrotopSudsAsciiDescriptor.addSuds( suds.getElementType(), sudsNaturalAreaPart, sudsDrainedSealedAreaPart );
                  hydrotopNaturalArea -= sudsNaturalAreaPart;
                  hydrotopSealedArea -= sudsDrainedSealedAreaPart;
                  totalSudsNaturalArea += sudsNaturalAreaPart;
                  totalSudsSealedArea += sudsDrainedSealedAreaPart;
                  m_conf.addSudsMaxPercRateMember( feature.getId(), hydrotop.getMaxPerkolationRate() );
                }
                else if( feature instanceof IGreenRoof )
                {
                  final IGreenRoof suds = (IGreenRoof) feature;
                  final double areaRate = suds.getAreaPercentage() / 100.0;
                  final double sealedAreaPart = hydrotopSealedAreaAfterUnsealing * areaRate;
                  hydrotopSudsAsciiDescriptor.addSuds( suds.getElementType(), sealedAreaPart );
                  hydrotopSealedArea -= sealedAreaPart;
                  totalSudsSealedArea += sealedAreaPart;
                  m_conf.addSudsMaxPercRateMember( feature.getId(), hydrotop.getMaxPerkolationRate() );
                }
              }
              anySuds = true;
            }
            // final double sealingRate = hydrotopSealedArea / (hydrotopArea - );
            if( hydrotopNaturalArea < 0.0 )
              throw new SimulationException( "Hydrotop natural area is less than 0.0 m2." ); //$NON-NLS-1$
            hydrotopOutputList.add( String.format( Locale.US, "%-10.3f %-10s %-10s %-10.3g %-10.3g %-10d %-10.3f %-1d %s", hydrotopNaturalArea, m_conf.getLanduseFeatureShortedName( hydrotop.getLanduse() ), soilType, hydrotop.getMaxPerkolationRate(), hydrotop.getGWFactor(), ++hydrotopAsciiID, totalSealingPercentage, (sudsCollection.size() > 0) ? 1 : 0, hydrotopSudsAsciiDescriptor.getAscii() ) ); //$NON-NLS-1$
            totalHydrotopArea += hydrotopArea;
            totalHydrotopSealedArea += hydrotopSealedArea;
            totalHydrotopNaturalArea += hydrotopNaturalArea;

            m_conf.addHydrotopMapping( catchmentAsciiID, hydrotopAsciiID, hydrotop );
            hydIdList.add( hydrotop.getId() );
          }
        }

        // TODO: throw exception (to the user), if writing of hydrotope file is checked (testing!!!)
        final double fehler = Math.abs( catchmentGeometry.getArea() - totalHydrotopArea );
        final double fehlerinProzent = 100.0 * fehler / totalHydrotopArea;
        if( fehlerinProzent > 1.0 )
          m_logger.log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.HydrotopManager.3", totalHydrotopArea, catchmentFE.getId(), catchmentGeometry.getArea(), fehler, fehlerinProzent ) ); //$NON-NLS-1$

        if( anySuds )
        {
          asciiBuffer.getHydBuffer().append( String.format( Locale.US, "%d %d %.3f %.3f %.3f 1 %.3f %.3f %.3f\n", catchmentAsciiID, hydrotopOutputList.size(), totalHydrotopSealedArea, totalHydrotopNaturalArea, totalHydrotopSealedArea + totalHydrotopNaturalArea, totalSudsSealedArea, totalSudsNaturalArea, totalSudsSealedArea + totalSudsNaturalArea ) ); //$NON-NLS-1$
        }
        else
        {
          asciiBuffer.getHydBuffer().append( String.format( Locale.US, "%d %d %g %g %g\n", catchmentAsciiID, hydrotopOutputList.size(), totalHydrotopSealedArea, totalHydrotopNaturalArea, totalHydrotopSealedArea + totalHydrotopNaturalArea ) ); //$NON-NLS-1$
        }
        for( final String line : hydrotopOutputList )
          asciiBuffer.getHydBuffer().append( line ).append( "\n" ); //$NON-NLS-1$

        m_conf.getHydroHash().addHydroInfo( catchmentFE, hydIdList );
      }
    }
  }
}
