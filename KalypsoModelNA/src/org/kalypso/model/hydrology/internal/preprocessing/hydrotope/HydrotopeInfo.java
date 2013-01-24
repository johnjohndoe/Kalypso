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
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.suds.IAbstractSwale;
import org.kalypso.model.hydrology.binding.suds.IGreenRoof;
import org.kalypso.model.hydrology.binding.suds.ISealing;
import org.kalypso.model.hydrology.binding.suds.ISuds;
import org.kalypso.model.hydrology.binding.suds.ISudsWithElementType;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * Wrapper that holds {@link IHydrotope} with additional info about it's suds.
 * 
 * @author Gernot Belger
 */
public class HydrotopeInfo
{
  private final Map<String, Sealing> m_sudsSealingMap = new HashMap<String, Sealing>();

  private final IHydrotope m_hydrotop;

  /** Original hydrotope sealing changed by {@link ISealing}'s */
  private Sealing m_hydrotopSealingAfterUnsealing;

  /** Total sealing including changes by all {@link ISuds}. */
  private Sealing m_sealingWithSuds;

  private Sealing m_sudsSealing;

  private final LanduseHash m_landuseHash;

  private final int m_localID;

  private boolean m_hasSudsWithElementType = false;

  public HydrotopeInfo( final IHydrotope hydrotop, final LanduseHash landuseHash, final int localID )
  {
    m_hydrotop = hydrotop;
    m_landuseHash = landuseHash;
    m_localID = localID;

    /* We allow for only one suds-type per hydrotope. */
    m_sudsSealingMap.put( "10", new Sealing( 0.0, Double.NaN ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "11", new Sealing( 0.0, Double.NaN ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "12", new Sealing( 0.0, Double.NaN ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "13", new Sealing( 0.0, Double.NaN ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "20", new Sealing( 0.0, 0.0 ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "21", new Sealing( 0.0, 0.0 ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "30", new Sealing( 0.0, 0.0 ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "31", new Sealing( 0.0, 0.0 ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "40", new Sealing( 0.0, Double.NaN ) ); //$NON-NLS-1$
    m_sudsSealingMap.put( "41", new Sealing( 0.0, Double.NaN ) ); //$NON-NLS-1$    
  }

  public int getLocalID( )
  {
    return m_localID;
  }

  public IHydrotope getHydrotop( )
  {
    return m_hydrotop;
  }

  private double getSealingRate( ) throws SimulationException
  {
    final String landuseName = m_hydrotop.getLanduse();
    final Double landuseSealing = m_landuseHash.getSealingRate( landuseName );
    if( landuseSealing == null )
    {
      final String msg = String.format( "Unknown landuse: '%s'. Please recreate hydrotopes!", landuseName ); //$NON-NLS-1$
      throw new SimulationException( msg );
    }

    return landuseSealing.doubleValue();
  }


  public void calculateSealing( ) throws SimulationException
  {
    final GM_MultiSurface geometry = m_hydrotop.getGeometry();
    final double hydrotopArea = geometry.getArea();
    final double landuseSealing = getSealingRate();

    final double corrSealing = m_hydrotop.getCorrSealing();
    // FIXME:
    // HOTFIX: should actually lead to an exception instead: planer client produces corrSeling > 1.0
    final double corrSealingFixed = Math.abs( Math.min( corrSealing, 1.0 ) );

    final double totalSealingFaktor = landuseSealing * corrSealingFixed;
    if( totalSealingFaktor > 1.0 )
      throw new SimulationException( String.format( "Hydrotop has a sealing faktor > 1.0: %.2f", totalSealingFaktor ) ); //$NON-NLS-1$

    final Sealing originalHydrotopSealing = Sealing.createFromSealing( hydrotopArea, totalSealingFaktor );

    /* First init unsealed value, needed for suds-sealing */
    m_hydrotopSealingAfterUnsealing = calculateSealingWithUnsealing( originalHydrotopSealing );

    m_sudsSealing = calculateSudsSealing();

    m_sealingWithSuds = m_hydrotopSealingAfterUnsealing.substract( m_sudsSealing );

    final double naturalArea = m_sealingWithSuds.getNaturalArea();
    if( naturalArea < 0.0 )
      throw new SimulationException( String.format( "Hydrotop natural area is less than 0.0 m2: %.4f", naturalArea ) ); //$NON-NLS-1$
  }

  private Sealing calculateSealingWithUnsealing( final Sealing originalHydrotopSealing )
  {
    Sealing hydrotopSealingAfterUnsealing = originalHydrotopSealing;

    // first, fix the sealing
    final IFeatureBindingCollection<ISuds> sudsCollection = m_hydrotop.getSudCollection();
    for( final ISuds suds : sudsCollection )
    {
      if( suds instanceof ISealing )
      {
        final ISealing sealingSuds = (ISealing) suds;
        hydrotopSealingAfterUnsealing = hydrotopSealingAfterUnsealing.changeSealingByFactor( sealingSuds.getSealingFactor() );
      }
    }

    return hydrotopSealingAfterUnsealing;
  }

  private Sealing calculateSudsSealing( )
  {
    // than, apply other measures
    Sealing sudsSealingTotal = new Sealing();

    final IFeatureBindingCollection<ISuds> sudsCollection = m_hydrotop.getSudCollection();
    for( final ISuds suds : sudsCollection )
    {
      if( suds instanceof ISudsWithElementType )
      {
        final ISudsWithElementType sudsWithType = (ISudsWithElementType) suds;
        final Sealing sudsSealing = getSudsSealing( sudsWithType );
        sudsSealingTotal = sudsSealingTotal.add( sudsSealing );
        categorizeSuds( sudsWithType, sudsSealing );
      }
    }

    return sudsSealingTotal;
  }

  /** Categorize the suds by its types. There can only be one per type */
  private void categorizeSuds( final ISudsWithElementType suds, final Sealing sudsSealing )
  {
    final String elementType = suds.getElementType();
    m_sudsSealingMap.put( elementType, sudsSealing );
    m_hasSudsWithElementType = true;
  }

  public Sealing getSudsSealing( final ISudsWithElementType suds )
  {
    if( suds instanceof IAbstractSwale )
    {
      final IAbstractSwale swale = (IAbstractSwale) suds;

      final double drainingRateOfSealedArea = swale.getDrainedPercentageOfSealedArea() / 100.0;
      final double sealedAreaPart = m_hydrotopSealingAfterUnsealing.getSealedArea() * drainingRateOfSealedArea;

      final double naturalAreaRate = swale.getNaturalAreaPercentage() / 100.0;
      final double naturalAreaPart = m_hydrotopSealingAfterUnsealing.getNaturalArea() * naturalAreaRate;

      return new Sealing( sealedAreaPart, naturalAreaPart );
    }

    if( suds instanceof IGreenRoof )
    {
      final IGreenRoof greenroof = (IGreenRoof) suds;

      final double areaRate = greenroof.getAreaPercentage() / 100.0;
      final double sealedAreaPart = m_hydrotopSealingAfterUnsealing.getSealedArea() * areaRate;
      return new Sealing( sealedAreaPart, Double.NaN );
    }

    throw new IllegalArgumentException();
  }

  public boolean hasSudsWithElementType( )
  {
    return m_hasSudsWithElementType;
  }

  /**
   * Returns the total sealing areas of all suds.
   */
  public Sealing getSudsSealing( )
  {
    return m_sudsSealing;
  }

  public Sealing getHydrotopeSealingMinusSuds( )
  {
    return m_sealingWithSuds;
  }

  public Sealing getHydrotopSealingAfterUnsealing( )
  {
    return m_hydrotopSealingAfterUnsealing;
  }

  public Sealing getSudsSealingByType( final String sudsElementType )
  {
    return m_sudsSealingMap.get( sudsElementType );
  }
}
