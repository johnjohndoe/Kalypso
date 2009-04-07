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
package org.kalypso.convert.namodel.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.schema.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for rrmSoilType:SoilTypeCollection's
 * 
 * @author Gernot Belger
 */
public class SoilTypeCollection extends Feature_Impl
{
  public static final QName QNAME_PROP_SOILTYPEMEMBER = new QName( NaModelConstants.NS_NAPEDOLOGIE, "soiltypeMember" );

  private final IFeatureBindingCollection<SoilType> m_soilTypes;

  public SoilTypeCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_soilTypes = new FeatureBindingCollection<SoilType>( this, SoilType.class, QNAME_PROP_SOILTYPEMEMBER );
  }

  public IFeatureBindingCollection<SoilType> getSoilTypes( )
  {
    return m_soilTypes;
  }

  /**
   * Create/Import a new soilType into this collection.
   * 
   * @return <code>null</code> if the given geometry is <code>null</code>.
   */
  public SoilType importSoilType( final String label, final GM_MultiSurface geometry, final ImportType importType, final List<IStatus> log )
  {
    if( geometry == null )
      return null;

    // Handle existing soilTypes that intersect the new one
    final List<SoilType> existingSoilTypes = m_soilTypes.query( geometry.getEnvelope() );
    for( final SoilType existingPedology : existingSoilTypes )
    {
      switch( importType )
      {
        case DELETE_INTERSECTING:
        {
          m_soilTypes.remove( existingPedology );
          final String message = String.format( "SoilType '%s' was deleted due to intersection", existingPedology.getId() );
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }
          break;

        case IGNORE_INTERSECTING:
        {
          final String message = String.format( "Ingoring imported soilType '%s' due to intersection", label );
          log.add( StatusUtilities.createStatus( IStatus.WARNING, message, null ) );
        }
          return null;

        case INTERSECT:
        {
          final GM_MultiSurface existingGeometry = existingPedology.getGeometry();
          final GM_MultiSurface difference = PolygonIntersectionHelper.createDifference( geometry, existingGeometry );
          if( difference != null )
          {
            existingPedology.setGeometry( difference );
            final String message = String.format( "SoilType '%s' was reduced by imported soilType '%s'", existingPedology.getId(), label );
            log.add( StatusUtilities.createStatus( IStatus.INFO, message, null ) );
          }
          else
          {
            m_soilTypes.remove( existingPedology );
            final String message = String.format( "SoilType '%s' was removed by imported soilType '%s'", existingPedology.getId(), label );
            log.add( StatusUtilities.createStatus( IStatus.INFO, message, null ) );
          }
        }

        case CLEAR_OUTPUT:
          // nothing to do, we add all soilTypes
          break;
      }
    }

    // Create new soilType
    final SoilType pedology = m_soilTypes.addNew( SoilType.QNAME );
    pedology.setName( label );
    pedology.setGeometry( geometry );
    return pedology;
  }
}
