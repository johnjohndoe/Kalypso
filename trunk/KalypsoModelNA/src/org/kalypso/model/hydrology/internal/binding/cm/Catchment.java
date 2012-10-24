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
package org.kalypso.model.hydrology.internal.binding.cm;

import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * The catchment.
 *
 * @author Holger Albert
 */
public class Catchment extends Feature_Impl implements ICatchment
{
  private final IFeatureBindingCollection<IFactorizedTimeseries> m_timeseries;

  public Catchment( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_timeseries = new FeatureBindingCollection<>( this, IFactorizedTimeseries.class, MEMBER_FACTORIZED_TIMESERIES );
  }

  @Override
  public Feature getAreaLink( )
  {
    return getProperty( PROPERTY_AREA_LINK, Feature.class );
  }

  @Override
  public void setAreaLink( final String href )
  {
    setLink( PROPERTY_AREA_LINK, href );
  }

  @Override
  public IFeatureBindingCollection<IFactorizedTimeseries> getFactorizedTimeseries( )
  {
    return m_timeseries;
  }

  @Override
  public ILinearSumGenerator getOwner( )
  {
    return (ILinearSumGenerator) super.getOwner();
  }

  @Override
  public GM_Polygon resolveArea( )
  {
    final ILinearSumGenerator generator = getOwner();

    /* Get the area property. */
    final GMLXPath areaPath = generator.getAreaPath();
    if( areaPath == null )
      return null;

    /* Check if catchment exists. */
    final Feature catchment = getAreaLink();
    if( catchment == null )
      return null;

    final Object area = GMLXPathUtilities.queryQuiet( areaPath, catchment );
    if( area instanceof GM_Polygon )
      return (GM_Polygon) area;

    return null;
  }

  @Override
  public String resolveName( )
  {
    final ILinearSumGenerator parent = getOwner();

    /* Get the name property. */
    final GMLXPath namePath = parent.getAreaNamePath();
    if( namePath == null )
      return null;

    /* Check if catchment exists. */
    final Feature catchment = getAreaLink();
    if( catchment == null )
      return null;

    final Object name = GMLXPathUtilities.queryQuiet( namePath, catchment );
    if( name instanceof String )
      return (String) name;

    if( name instanceof List )
    {
      final List< ? > names = (List< ? >) name;
      if( names.size() == 0 )
        return null;

      return (String) names.get( 0 );
    }

    return null;
  }

  @Override
  public String resolveDescription( )
  {
    final ILinearSumGenerator parent = getOwner();

    /* Get the description property. */
    final GMLXPath descriptionPath = parent.getAreaDescriptionPath();
    if( descriptionPath == null )
      return null;

    /* Check if catchment exists. */
    final Feature catchment = getAreaLink();
    if( catchment == null )
      return null;

    /* Query. */
    final Object description = GMLXPathUtilities.queryQuiet( descriptionPath, catchment );
    if( description instanceof String )
      return (String) description;

    return null;
  }

}