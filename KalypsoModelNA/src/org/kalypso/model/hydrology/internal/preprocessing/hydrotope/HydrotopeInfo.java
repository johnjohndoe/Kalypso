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

import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
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

  private Sealing m_sealing;

  public HydrotopeInfo( final IHydrotope hydrotop, final ParameterHash landuseHash, final int localID )
  {
    m_hydrotope = hydrotop;
    m_landuseHash = landuseHash;
    m_localID = localID;
  }

  public Sealing getSealing( ) throws NAPreprocessorException
  {
    if( m_sealing == null )
      m_sealing = calculateSealing();

    return m_sealing;
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

  private String getLanduseClassName( )
  {
    final IXLinkedFeature landuseLink = m_hydrotope.getLanduseLink();
    if( landuseLink != null )
    {
      final Landuse landuse = (Landuse) landuseLink.getFeature();
      return landuse.getLanduseClassName();
    }

    return m_hydrotope.getLanduse();
  }

  private double getLanduseSealingRate( ) throws NAPreprocessorException
  {
    final String landuseName = getLanduseClassName();
    final Double landuseSealing = m_landuseHash.getSealingRate( landuseName );
    if( landuseSealing == null )
    {
      final String msg = String.format( "Unknown landuse: '%s' for hydrotope '%s'.", landuseName, getName() ); //$NON-NLS-1$
      throw new NAPreprocessorException( msg );
    }

    return landuseSealing.doubleValue();
  }

  public double getMaxPerkolationRate( ) throws NAPreprocessorException
  {
    final IXLinkedFeature geologyLink = m_hydrotope.getGeologyLink();
    if( geologyLink != null )
    {
      final Geology geology = (Geology) geologyLink.getFeature();
      final Double maxPercolationRate = geology.getMaxPercolationRate();

      if( maxPercolationRate == null )
      {
        final String msg = String.format( "Max. percolation rate not set in geology for hydrotope: '%s'.", getName() ); //$NON-NLS-1$
        throw new NAPreprocessorException( msg );
      }

      return maxPercolationRate;
    }

    final Double maxPercolationRate = m_hydrotope.getMaxPerkolationRate();
    if( maxPercolationRate != null )
      return maxPercolationRate;

    final String msg = String.format( "Max. percolation rate not set for hydrotope: '%s'.", getName() ); //$NON-NLS-1$
    throw new NAPreprocessorException( msg );
  }

  public double getGWFactor( ) throws NAPreprocessorException
  {
    final IXLinkedFeature geologyLink = m_hydrotope.getGeologyLink();
    if( geologyLink != null )
    {
      final Geology geology = (Geology) geologyLink.getFeature();
      final Double gwFactor = geology.getGWFactor();

      if( gwFactor == null )
      {
        final String msg = String.format( "Groundwater inflwo rate not set in geology for hydrotope: '%s'.", getName() ); //$NON-NLS-1$
        throw new NAPreprocessorException( msg );
      }

      return gwFactor;
    }

    final Double gwFactor = m_hydrotope.getGWFactor();
    if( gwFactor != null )
      return gwFactor;

    final String msg = String.format( "Groundwater inflwo rate not set for hydrotope: '%s'.", getName() ); //$NON-NLS-1$
    throw new NAPreprocessorException( msg );
  }

  public Soiltype findSoiltype( ) throws NAPreprocessorException
  {
    final IXLinkedFeature pedologyLink = m_hydrotope.getPedologyLink();
    if( pedologyLink != null )
    {
      final SoilType pedology = (SoilType) pedologyLink.getFeature();
      final IXLinkedFeature soilTypeLink = pedology.getSoilType();
      if( soilTypeLink != null )
        return (Soiltype) soilTypeLink.getFeature();

      final String msg = String.format( "Soil type not set in pedology for hydrotope: '%s'.", getName() ); //$NON-NLS-1$
      throw new NAPreprocessorException( msg );
    }

    final String soilTypeID = m_hydrotope.getSoilType();
    final Soiltype soiltype = m_landuseHash.getSoilType( soilTypeID );
    if( soiltype != null )
      return soiltype;

    final String msg = String.format( Messages.getString( "HydrotopeWriter.0" ), soilTypeID, getName() ); //$NON-NLS-1$
    throw new NAPreprocessorException( msg );
  }

  private double getHydrotopeArea( ) throws NAPreprocessorException
  {
    final GM_MultiSurface geometry = m_hydrotope.getGeometry();
    if( geometry != null )
      return geometry.getArea();

    final Double area = m_hydrotope.getArea();
    if( area != null )
      return area;

    final String msg = String.format( "Failed to determine area for hydrotope '%s'", getName() );
    throw new NAPreprocessorException( msg );
  }

  private double getSealingCorrection( ) throws NAPreprocessorException
  {
    final IXLinkedFeature catchmentLink = m_hydrotope.getCatchmentLink();
    final IXLinkedFeature landuseLink = m_hydrotope.getLanduseLink();
    if( catchmentLink != null && landuseLink != null )
    {
      final Catchment catchment = (Catchment) catchmentLink.getFeature();
      final Landuse landuse = (Landuse) landuseLink.getFeature();

      final double catchmentCorrSealing = catchment.getCorrSealing();
      final double landuseCorrSealing = landuse.getCorrSealing();

      return catchmentCorrSealing * landuseCorrSealing;
    }

    final Double corrSealing = m_hydrotope.getCorrSealing();
    if( corrSealing != null )
      return corrSealing;

    final String msg = String.format( "Failed to determine sealing correction factor for hydrotope %s", getName() );
    throw new NAPreprocessorException( msg );
  }

  private Sealing calculateSealing( ) throws NAPreprocessorException
  {
    final double hydrotopArea = getHydrotopeArea();

    final double landuseSealing = getLanduseSealingRate();

    final double corrSealing = getSealingCorrection();

    final double totalSealingFaktor = landuseSealing * corrSealing;

    if( totalSealingFaktor < 0.0 || totalSealingFaktor > 1.0 )
      throw new NAPreprocessorException( String.format( "Total sealing %.2f outside valid range (0.0 - 1.0) for hydrotope %s.", totalSealingFaktor, getName() ) );

    return Sealing.createFromSealing( hydrotopArea, totalSealingFaktor );
  }

  public String getLanduseShortName( )
  {
    final String landuseClassName = getLanduseClassName();

    return m_landuseHash.getLanduseFeatureShortedName( landuseClassName );
  }
}