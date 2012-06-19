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
package org.kalypso.model.hydrology.internal.preprocessing.hydrotope;

import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.OverlayElement;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.parameter.Soiltype;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;

/**
 * Wrapper that holds {@link IHydrotope}.
 * 
 * @author Gernot Belger
 */
public class HydrotopeInfo
{
  private final IHydrotope m_hydrotope;

  private final ParameterHash m_landuseHash;

  private final int m_localID;

  private final double m_gwFactor;

  private final double m_maxPerc;

  private final String m_landuseClassName;

  private final double m_totalSealingRate;

  private double m_area;

  private final Soiltype m_soilType;

  public HydrotopeInfo( final IHydrotope hydrotop, final ParameterHash landuseHash, final int localID ) throws NAPreprocessorException
  {
    m_hydrotope = hydrotop;
    m_landuseHash = landuseHash;
    m_localID = localID;
    m_area = calculateHydrotopeArea();

    m_gwFactor = calculateGWFactor();
    m_maxPerc = calculateMaxPerkolationRate();
    m_landuseClassName = calculateLanduseClassName();
    m_totalSealingRate = calculateTotalSealingFactor();
    m_soilType = findSoiltype();

    validateAttributes();
  }

  public String getFeatureId( )
  {
    return m_hydrotope.getId();
  }

  public String getName( )
  {
    return m_hydrotope.getName();
  }

  public int getLocalID( )
  {
    return m_localID;
  }

  public double getGwFactor( )
  {
    return Math.max( 0.0, Math.min( 1.0, m_gwFactor ) );
  }

  public double getMaxPercolationRate( )
  {
    return m_maxPerc;
  }

  public Soiltype getSoilType( )
  {
    return m_soilType;
  }

  private String calculateLanduseClassName( )
  {
    final Landuse linkedLanduse = getLinkedLanduse();

    if( linkedLanduse != null )
      return linkedLanduse.getLanduseClassName();

    return m_hydrotope.getLanduse();
  }

  private double getLanduseSealingRate( ) throws NAPreprocessorException
  {
    final Double landuseSealing = m_landuseHash.getSealingRate( m_landuseClassName );
    if( landuseSealing == null )
    {
      final String msg = String.format( "Unknown landuse: '%s' for hydrotope '%s'.", m_landuseClassName, getName() );
      throw new NAPreprocessorException( msg );
    }

    return landuseSealing.doubleValue();
  }

  private double calculateMaxPerkolationRate( ) throws NAPreprocessorException
  {
    final double maxPercCorrection = getMaxPercCorrection();

    final Geology linkedGeology = getLinkedGeology();
    if( linkedGeology != null )
    {
      final Double maxPercolationRate = linkedGeology.getMaxPercolationRate();

      if( maxPercolationRate == null )
      {
        final String msg = String.format( "Max. percolation rate not set in geology for hydrotope: '%s'.", getName() );
        throw new NAPreprocessorException( msg );
      }

      return maxPercolationRate * maxPercCorrection;
    }

    final Double maxPercolationRate = m_hydrotope.getMaxPerkolationRate();
    if( maxPercolationRate != null )
      return maxPercolationRate * maxPercCorrection;

    final String msg = String.format( "Max. percolation rate not set for hydrotope: '%s'.", getName() );
    throw new NAPreprocessorException( msg );
  }

  private double getMaxPercCorrection( )
  {
    final Catchment linkedCatchment = getLinkedCatchment();
    if( linkedCatchment == null )
      return 1.0;

    return linkedCatchment.getCorrMaxPercolation();
  }

  private double calculateGWFactor( ) throws NAPreprocessorException
  {
    final double gwInflowCorrection = getGwInflowCorrection();

    final Geology linkedGeology = getLinkedGeology();
    if( linkedGeology != null )
    {
      final Double gwFactor = linkedGeology.getGWFactor();

      if( gwFactor == null )
      {
        final String msg = String.format( "Groundwater inflwo rate not set in geology for hydrotope: '%s'.", getName() );
        throw new NAPreprocessorException( msg );
      }

      return gwFactor * gwInflowCorrection;
    }

    final Double gwFactor = m_hydrotope.getGWFactor();
    if( gwFactor != null )
      return gwFactor * gwInflowCorrection;

    final String msg = String.format( "Groundwater inflwo rate not set for hydrotope: '%s'.", getName() );
    throw new NAPreprocessorException( msg );
  }

  private double getGwInflowCorrection( )
  {
    final Catchment linkedCatchment = getLinkedCatchment();
    if( linkedCatchment == null )
      return 1.0;

    return linkedCatchment.getCorrGwInflowRate();
  }

  private Soiltype findSoiltype( ) throws NAPreprocessorException
  {
    final SoilType linkedPedology = getLinkedPedology();
    if( linkedPedology != null )
    {
      final IXLinkedFeature soilTypeLink = linkedPedology.getSoilType();
      if( soilTypeLink != null )
        return (Soiltype) soilTypeLink.getFeature();

      final String msg = String.format( "Soil type not set in pedology for hydrotope: '%s'.", getName() );
      throw new NAPreprocessorException( msg );
    }

    final String soilTypeID = m_hydrotope.getSoilType();
    final Soiltype soiltype = m_landuseHash.getSoilType( soilTypeID );
    if( soiltype != null )
      return soiltype;

    final String msg = String.format( Messages.getString( "HydrotopeWriter.0" ), soilTypeID, getName() ); //$NON-NLS-1$
    throw new NAPreprocessorException( msg );
  }

  private double calculateHydrotopeArea( ) throws NAPreprocessorException
  {
    final GM_MultiSurface geometry = m_hydrotope.getGeometry();
    if( geometry != null )
      return geometry.getArea();

    final Double area = m_hydrotope.getArea();
    if( area != null )
      return area;

    final String msg = String.format( Messages.getString("HydrotopeInfo.0"), getName() ); //$NON-NLS-1$
    throw new NAPreprocessorException( msg );
  }

  private double getSealingCorrection( ) throws NAPreprocessorException
  {
    final Landuse linkedLanduse = getLinkedLanduse();
    final Catchment linkedCatchment = getLinkedCatchment();

    if( linkedCatchment != null && linkedLanduse != null )
    {
      final double catchmentCorrSealing = linkedCatchment.getCorrSealing();
      final double landuseCorrSealing = linkedLanduse.getCorrSealing();

      return catchmentCorrSealing * landuseCorrSealing;
    }

    final Double corrSealing = m_hydrotope.getCorrSealing();
    if( corrSealing == null )
    {
      final String msg = String.format( Messages.getString( "HydrotopeInfo.1" ), getName() ); //$NON-NLS-1$
      throw new NAPreprocessorException( msg );
    }

    return corrSealing;
  }

  private double calculateTotalSealingFactor( ) throws NAPreprocessorException
  {
    final double landuseSealing = getLanduseSealingRate();

    final double corrSealing = getSealingCorrection();

    return landuseSealing * corrSealing;
  }

  public String getLanduseShortName( )
  {
    final String landuseClassName = calculateLanduseClassName();

    return m_landuseHash.getLanduseFeatureShortedName( landuseClassName );
  }

  public void addArea( final HydrotopeInfo hydrotopInfo )
  {
    m_area += hydrotopInfo.m_area;
  }

  public String getAttributeHash( )
  {
    final StringBuilder buffer = new StringBuilder();

    buffer.append( m_gwFactor );
    buffer.append( '#' );
    buffer.append( m_landuseClassName );
    buffer.append( '#' );
    buffer.append( m_maxPerc );
    buffer.append( '#' );
    buffer.append( m_totalSealingRate );
    buffer.append( '#' );
    buffer.append( m_soilType.getName() );

    return buffer.toString();
  }

  public double getTotalSealingRate( )
  {
    return m_totalSealingRate;
  }

  public double getNaturalArea( )
  {
    return m_area * (1 - m_totalSealingRate);
  }

  public Sealing createSealing( )
  {
    return Sealing.createFromSealing( m_area, m_totalSealingRate );
  }

  private Landuse getLinkedLanduse( )
  {
    final IXLinkedFeature landuseLink = m_hydrotope.getLanduseLink();
    if( landuseLink == null )
      return null;

    return (Landuse) landuseLink.getFeature();
  }

  private Catchment getLinkedCatchment( )
  {
    final IXLinkedFeature catchmentLink = m_hydrotope.getCatchmentLink();
    if( catchmentLink == null )
      return null;

    return (Catchment) catchmentLink.getFeature();
  }

  private Geology getLinkedGeology( )
  {
    final IXLinkedFeature geologyLink = m_hydrotope.getGeologyLink();
    if( geologyLink == null )
      return null;

    return (Geology) geologyLink.getFeature();
  }

  private SoilType getLinkedPedology( )
  {
    final IXLinkedFeature pedologyLink = m_hydrotope.getPedologyLink();
    if( pedologyLink == null )
      return null;

    return (SoilType) pedologyLink.getFeature();
  }

  private OverlayElement getLinkedOverlay( )
  {
    final IXLinkedFeature overlayLink = m_hydrotope.getOverlayLink();
    if( overlayLink == null )
      return null;

    return (OverlayElement) overlayLink.getFeature();
  }

  public void validateAttributes( ) throws NAPreprocessorException
  {
    if( m_gwFactor < 0.0 || m_gwFactor > 1.0 )
    {
      final String message = String.format( "Total groundwater inflow rate %.2f (including catchment correction factor) outside valid range (0.0 - 1.0) for hydrotope %s.", m_gwFactor, getName() );
      throw new NAPreprocessorException( message );
    }

    if( m_maxPerc < 0.0 )
    {
      final String message = String.format( "Maximal percolation %.2f (including catchment correction factor) outside valid range (0.0 - infinity) for hydrotope %s.", m_maxPerc, getName() );
      throw new NAPreprocessorException( message );
    }

    // REMARK: sealing of 1.0 is not allowed, because the calculation core cannot handle this:
    // it calculates the sealed area as sealingRate * naturalArea / (1 - sealingRate );
    // so we get a division by zero here.
    final double maxSealing = 9e-10;

    if( m_totalSealingRate < 0.0 || m_totalSealingRate > maxSealing )
      throw new NAPreprocessorException( String.format( Messages.getString( "HydrotopeInfo.2" ), m_totalSealingRate, maxSealing, getName() ) ); //$NON-NLS-1$
  }
}