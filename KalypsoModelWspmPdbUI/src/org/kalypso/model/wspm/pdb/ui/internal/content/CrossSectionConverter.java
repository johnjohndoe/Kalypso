/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.utils.ConsecutiveNumComparator;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Gernot Belger
 */
public class CrossSectionConverter
{
  private final CrossSection m_section;

  private final IProfil m_profile;

  public CrossSectionConverter( final CrossSection section, final IProfil profile )
  {
    m_section = section;
    m_profile = profile;
  }

  public void execute( )
  {
    m_profile.setName( m_section.getName() );
    m_profile.setDescription( m_section.getDescription() );

    final BigDecimal station = m_section.getStation();
    /* [m] -> [km] */
    m_profile.setStation( station.movePointLeft( 4 ).doubleValue() );

    // TODO convert section parts, starting with kind 'P'
    convertP();

    // TODO: other kinds...
    // Bridge
    // Weir
    // Other building types (tubes, ...)
  }

  private void convertP( )
  {
    final CrossSectionPart part = m_section.findPartByCategory( IGafConstants.KZ_CATEGORY_PROFILE );
    if( part == null )
      return;

    // TODO: put part's name, description etc. into profile objects

    final TupleResult result = m_profile.getResult();

    /* Add points in their natural order into the profile */
    final Set<Point> points = part.getPoints();
    final List<Point> sortedPoints = sortPoints( points );
    for( final Point point : sortedPoints )
    {
      final IRecord record = result.createRecord();
      result.add( record );

      setValue( record, IWspmConstants.POINT_PROPERTY_COMMENT, point.getDescription() );
      setValue( record, IWspmConstants.POINT_PROPERTY_BREITE, asDouble( point.getWidth() ) );
      setValue( record, IWspmConstants.POINT_PROPERTY_HOEHE, asDouble( point.getHight() ) );

      final com.vividsolutions.jts.geom.Point location = point.getLocation();
      if( location != null )
      {
        setValue( record, IWspmConstants.POINT_PROPERTY_RECHTSWERT, location.getX() );
        setValue( record, IWspmConstants.POINT_PROPERTY_HOCHWERT, location.getY() );
      }

      // TODO: add roughness class component
      // setValue( record, IWspmConstants.POINT_PROPERTY_, point.getRoughness() );
      setValue( record, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, point.getRoughnessKstValue() );
      setValue( record, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, point.getRoughnessKValue() );

      // TODO: add vegetation class component
      // setValue( record, IWspmConstants.POINT_PROPERTY_, point.getVegetation() );
      setValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, point.getVegetationAx() );
      setValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, point.getVegetationAy() );
      setValue( record, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, point.getVegetationDp() );

      // TODO: add corresponding components
      // setValue( record, IWspmConstants.POINT_PROPERTY_, point.getKz() );
      // setValue( record, IWspmConstants.POINT_PROPERTY_, point.getName() );
    }
  }

  private Double asDouble( final BigDecimal decimal )
  {
    if( decimal == null )
      return null;

    return decimal.doubleValue();
  }

  private void setValue( final IRecord record, final String componentID, final Object value )
  {
    if( value == null )
      return;

    final int component = ensureComponent( record, componentID );
    record.setValue( component, value );
  }

  private int ensureComponent( final IRecord record, final String componentID )
  {
    final TupleResult owner = record.getOwner();
    final int index = owner.indexOfComponent( componentID );
    if( index != -1 )
      return index;

    owner.addComponent( ProfilUtil.getFeatureComponent( componentID ) );
    return owner.indexOfComponent( componentID );
  }

  private List<Point> sortPoints( final Set<Point> points )
  {
    final ArrayList<Point> sortedPoints = new ArrayList<Point>( points );
    Collections.sort( sortedPoints, new ConsecutiveNumComparator() );
    return sortedPoints;
  }
}