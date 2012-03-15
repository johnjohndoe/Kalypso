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
package org.kalypso.model.hydrology.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.PolygonIntersectionHelper.ImportType;
import org.kalypso.model.hydrology.binding.suds.AbstractSud;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

/**
 * Binding class for rrmLanduse:LanduseCollection's
 *
 * @author Gernot Belger
 */
public class LanduseCollection extends UnversionedModel
{
  public static final QName QNAME_PROP_LANDUSEMEMBER = new QName( NaModelConstants.NS_NALANDUSE, "landuseMember" ); //$NON-NLS-1$

  private final IFeatureBindingCollection<Landuse> m_landuses;

  public LanduseCollection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_landuses = new FeatureBindingCollection<Landuse>( this, Landuse.class, QNAME_PROP_LANDUSEMEMBER );
  }

  public IFeatureBindingCollection<Landuse> getLanduses( )
  {
    return m_landuses;
  }

  /**
   * Create/Import a new landuse into this collection.
   *
   * @return <code>null</code> if the given geometry is <code>null</code>.
   */
  public void importLanduse( final ImportType importType, final String name, final GM_MultiSurface geometry, final String description, final Double corrSealing, final String drainageType, final String landuseRef, final AbstractSud[] suds )
  {
    // Handle existing landuses that intersect the new one
    final List<Landuse> existingLanduses = m_landuses.query( geometry.getEnvelope() );

    switch( importType )
    {
      case DIFFERENCE:
      {
        for( final Landuse existingLanduse : existingLanduses )
        {
          clipExistingLanduse( existingLanduse, geometry );
        }
        // go on to CLEAR_OUTPUT and add the new landuse
      }

      case CLEAR_OUTPUT:
      {
        final Landuse landuse = m_landuses.addNew( Landuse.QNAME );
        landuse.setName( name );
        landuse.setGeometry( geometry );
        landuse.setDescription( description );
        landuse.setCorrSealing( corrSealing );
        landuse.setDrainageType( drainageType );
        addLanduseLink( landuseRef, landuse );
        addSudsToLanduse( suds, landuse );
        return;
      }

      case UPDATE:
      {
        for( final Landuse existingLanduse : existingLanduses )
        {
          // first remember geometry, then clip
          final GM_MultiSurface existingGeometry = existingLanduse.getGeometry();
          clipExistingLanduse( existingLanduse, geometry );

          final GM_MultiSurface intersection = PolygonIntersectionHelper.createIntersection( geometry, existingGeometry );
          if( intersection != null && intersection.getArea() > 0.01 )
          {
            // clone existing landuse
            final Landuse landuse = m_landuses.addNew( Landuse.QNAME );
            landuse.setGeometry( intersection );

            if( corrSealing != null )
              landuse.setCorrSealing( corrSealing );
            else
              landuse.setCorrSealing( existingLanduse.getCorrSealing() );

            if( description != null )
              landuse.setDescription( description );
            else
              landuse.setDescription( existingLanduse.getDescription() );

            if( drainageType != null )
              landuse.setDrainageType( drainageType );
            else
              landuse.setDrainageType( existingLanduse.getDrainageType() );

            if( landuseRef != null )
              addLanduseLink( landuseRef, landuse );
            else
              landuse.setLanduse( existingLanduse.getLanduse() );

            if( name != null )
              landuse.setName( name );
            else
              landuse.setName( existingLanduse.getName() );

            if( suds != null )
              addSudsToLanduse( suds, landuse );
            else
              addSudsToLanduse( existingLanduse.getSuds(), landuse );
          }
        }
      }
    }
  }

  private void addLanduseLink( final String landuseRef, final Landuse landuse )
  {
    final String href = "parameter.gml#" + landuseRef; //$NON-NLS-1$
    final IFeatureType lcFT = GMLSchemaUtilities.getFeatureTypeQuiet( new QName( NaModelConstants.NS_NAPARAMETER, "Landuse" ) ); //$NON-NLS-1$
    landuse.setLink( Landuse.QNAME_PROP_LANDUSE, href, lcFT );
  }

  private void clipExistingLanduse( final Landuse existingLanduse, final GM_MultiSurface geometry )
  {
    final GM_MultiSurface existingGeometry = existingLanduse.getGeometry();
    final GM_MultiSurface difference = PolygonIntersectionHelper.createDifference( geometry, existingGeometry );
    if( difference != null && difference.getArea() > 0.01 )
    {
      existingLanduse.setGeometry( difference );
    }
    else
    {
      m_landuses.remove( existingLanduse );
    }
  }

  private void addSudsToLanduse( final Feature[] suds, final Landuse landuse )
  {
    final IFeatureBindingCollection<Feature> sudCollection = landuse.getSudCollection();

    for( final Feature sud : suds )
    {
      final String href = String.format( "suds.gml#%s", sud.getId() ); //$NON-NLS-1$
      sudCollection.addLink( href );
    }
  }
}
